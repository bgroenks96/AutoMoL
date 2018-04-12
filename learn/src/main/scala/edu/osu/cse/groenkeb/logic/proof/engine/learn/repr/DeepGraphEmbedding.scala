package edu.osu.cse.groenkeb.logic.proof.engine.learn.repr

import ml.dmlc.mxnet._
import ml.dmlc.mxnet.io.NDArrayIter
import ml.dmlc.mxnet.module.Module
import ml.dmlc.mxnet.optimizer.RMSProp

import edu.osu.cse.groenkeb.logic.proof.engine.learn.util.MxUtil
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemGraph
import ml.dmlc.mxnet.module.BucketingModule
import scala.collection.immutable.ListMap
import edu.osu.cse.groenkeb.logic.proof.engine.learn.GraphNode
import edu.osu.cse.groenkeb.logic.proof.engine.learn.SentenceGraph

final class DeepGraphEmbedding(hiddenDims: Int = 128) {
  private implicit val ctx = Context.defaultCtx
  private val embeddingDims = NodeEmbedding.embeddingSize
  private val inEdgeModule = initEdgeModule
  private val outEdgeModule = initEdgeModule
  private val updateModule = initUpdateModule
  private val maxPoolModule = initMaxPoolModule
  
  def embed(prob: ProblemGraph, steps: Int = 5, backGrad: Option[NDArray] = None) = {
    val graph = prob.graph
    val embeddings = graph.nodes.map(n => n -> new NodeEmbedding(n, graph.degree(n))).toMap
    for (i <- 0 until steps) {
      for ((v, xv) <- embeddings) {
        val inEdges = graph.adjIn(v).map(n => embeddings(n))
        val outEdges = graph.adjOut(v).map(n => embeddings(n))
        val xv_n = embeddingStep(xv, inEdges, outEdges)
        xv.push(xv_n)
      }
    }
    
    val batch = createDataBatch(embeddings.values.map(e => e.currentValue).toArray[NDArray])
    maxPoolModule.forward(batch, isTrain=Some(true))
    val graphEmbedding = maxPoolModule.getOutputsMerged().head
    backGrad match {
      case Some(grad) =>
        backward(grad, steps, embeddings)
        graphEmbedding
      case None =>
        graphEmbedding
    }
  }
  
  /**
   * Performs a forward pass on the embedding layers with the given node embeddings as input.
   * Returns the new embedding value NDArray as a result.
   */
  def embeddingStep(node: NodeEmbedding, inEdges: Seq[NodeEmbedding], outEdges: Seq[NodeEmbedding]) = {
    val vIn = NDArray.zeros(ctx, embeddingDims)
    if (inEdges.length > 0) {
      val uIns = NDArray.broadcast_to(Map("shape" -> (inEdges.length, embeddingDims)))(node.currentValue).get
      val ins = iter(inEdges)
      val inIter = new NDArrayIter(Array(ins, uIns))
      while (inIter.hasNext) {
        inEdgeModule.forward(inIter.next())
        vIn += inEdgeModule.getOutputsMerged().head
      }
    }
    
    val vOut = NDArray.zeros(ctx, embeddingDims)
    if (outEdges.length > 0) {
      val uOuts = NDArray.broadcast_to(Map("shape" -> (outEdges.length, embeddingDims)))(node.currentValue).get
      val outs = iter(outEdges)
      val outIter = new NDArrayIter(Array(uOuts, outs))
      while (outIter.hasNext) {
        inEdgeModule.forward(outIter.next())
        vOut += inEdgeModule.getOutputsMerged().head
      }
    }
    
    val deg = MxUtil.array(node.nodeDegree)
    try {
      val xv = node.currentValue
      val updateIter = MxUtil.iter(vIn, vOut, xv, deg)
      while (updateIter.hasNext) {
        updateModule.forward(updateIter.next())
      }
    } finally {
      deg.dispose()
    }
    
    updateModule.getOutputsMerged().head
  }
  
  def dispose {
    inEdgeModule.getSymbol.dispose()
    outEdgeModule.getSymbol.dispose()
    updateModule.getSymbol.dispose()
  }
  
  private def backward(grad: NDArray, steps: Int, embeddings: Map[GraphNode, NodeEmbedding]) {
    maxPoolModule.backward(Array(grad))
    val maxGradsWrtEmbeddings = maxPoolModule.getInputGradsMerged()
    val grads = maxGradsWrtEmbeddings.toArray
    for (i <- steps - 1 to 0 by -1) {
      var j = 0
      for ((v, xv) <- embeddings) {
        updateModule.backward(Array(grads(j)))
        val updateGradsWrtInputs = updateModule.getInputGradsMerged()
        val updateGradsWrtIn = updateGradsWrtInputs(0)
        val updateGradsWrtOut = updateGradsWrtInputs(1)
        val updateGradsWrtNode = updateGradsWrtInputs(2)
        inEdgeModule.backward(Array(updateGradsWrtIn))
        outEdgeModule.backward(Array(updateGradsWrtOut))
        updateModule.update()
        inEdgeModule.update()
        outEdgeModule.update()
        grads(j) = updateGradsWrtNode
        j += 1
      }
    }
  }
  
  private def iter(others: Seq[NodeEmbedding]) =
    others.map(n => n.currentValue).reduce[NDArray] { case (a1, a2) => NDArray.concatenate(a1, a2) }
  
  private def initEdgeModule: Module = {
    val xv = Symbol.Variable("xv")
    val xu = Symbol.Variable("xu")
    val concat = Symbol.Concat("cat")(xv, xu)(Map("dim" -> 0))
    val fc1 = Symbol.FullyConnected("fc1")(concat)(Map("num_hidden" -> hiddenDims))
    val relu1 = Symbol.Activation("relu1")(fc1)(Map("act_type" -> "relu"))
    val fc2 = Symbol.FullyConnected("fc2")(relu1)(Map("num_hidden" -> embeddingDims))
    val relu2 = Symbol.Activation("relu2")(fc2)(Map("act_type" -> "relu"))
    val module = new Module(relu2, dataNames=Array("xv", "xu"))
    val xvInputDesc = DataDesc("xv", Shape(1, embeddingDims), layout="NC")
    val xuInputDesc = DataDesc("xu", Shape(1, embeddingDims), layout="NC")
    module.bind(Array(xvInputDesc, xuInputDesc), forTraining=true)
    module.initParams(new Xavier())
    module.initOptimizer(optimizer=new RMSProp(learningRate=0.1f))
    module
  }
  
  private def initUpdateModule: Module = {
    val vIn = Symbol.Variable("vIn")
    val vOut = Symbol.Variable("vOut")
    val xv = Symbol.Variable("xv")
    val dv = Symbol.Variable("dv")
    val vSum = vIn + vOut
    val dvInv = Symbol.reciprocal("dvInv")(dv)()
    val prod = Symbol.broadcast_mul()(dvInv, vSum)()
    val newXv = xv + prod
    val fc1 = Symbol.FullyConnected("fc1")(newXv)(Map("num_hidden" -> hiddenDims))
    val relu1 = Symbol.Activation("relu1")(fc1)(Map("act_type" -> "relu"))
    val module = new Module(relu1, dataNames=Array("xv", "vIn", "vOut", "dv"))
    val vInInputDesc = DataDesc("vIn", Shape(1, embeddingDims), layout="NC")
    val vOutInputDesc = DataDesc("vOut", Shape(1, embeddingDims), layout="NC")
    val xvInputDesc = DataDesc("xv", Shape(1, embeddingDims), layout="NC")
    val dvInputDesc = DataDesc("dv", Shape(1, 1), layout="NC")
    module.bind(Array(vInInputDesc, vOutInputDesc, xvInputDesc, dvInputDesc), forTraining=true, inputsNeedGrad=true)
    module.initParams(new Xavier())
    module.initOptimizer(optimizer=new RMSProp(learningRate=0.1f))
    module
  }
  
  private def initMaxPoolModule: BucketingModule = {
    def symGen(key: AnyRef): (Symbol, IndexedSeq[String], IndexedSeq[String]) = {
      val len = key.asInstanceOf[Int]
      val syms = Array.ofDim[Symbol](len)
      val names = Array.ofDim[String](len)
      for (i <- 0 until len) {
        names(i) = "x_" + i
        syms(i) = Symbol.Variable(names(i))
      }
      val cc = Symbol.Concat()(syms:_*)(Map("dim" -> 0))
      val max = Symbol.max()(cc)(Map("axis" -> 0))
      (max, names, Array[String]())
    }
    val module = new BucketingModule(symGen, MxUtil.bkt(1))
    module.bind(Array(DataDesc("x_0", Shape(1,embeddingDims))), forTraining=true, inputsNeedGrad=true)
    module.initParams(new Xavier())
    module
  }
  
  private def createDataBatch(data: IndexedSeq[NDArray]) = {
    val size = data.length
    val dataShapes = ListMap(Array.range(0, size).map(i => s"x_$i" -> Shape(1, embeddingDims)):_*)
    val labelShapes = ListMap[String, Shape]()
    new DataBatch(data, Array[NDArray](), Array(0), 0, MxUtil.bkt(size), dataShapes, labelShapes)
  }
}