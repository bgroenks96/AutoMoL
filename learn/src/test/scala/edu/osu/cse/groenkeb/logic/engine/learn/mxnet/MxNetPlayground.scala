package edu.osu.cse.groenkeb.logic.engine.learn.mxnet

import ml.dmlc.mxnet._
import ml.dmlc.mxnet.module.Module
import ml.dmlc.mxnet.io.NDArrayIter
import ml.dmlc.mxnet.EvalMetric
import ml.dmlc.mxnet.optimizer.RMSProp
import ml.dmlc.mxnet.optimizer.SGD
import ml.dmlc.mxnet.module.BucketingModule
import scala.collection.immutable.ListMap

object MxNetPlayground {
  def main(args: Array[String]) = {
    testBucketing
    //testMultiLayerPerceptronXOR()
  }
  
  def testMultiLayerPerceptronXOR() = {
    val ctx = Context.cpu(0)
    val onehot = NDArray.onehotEncode(NDArray.array(Array(0), Shape(1), ctx), NDArray.zeros(1,10))
    println(onehot.toArray.mkString(" "))
    val input = Symbol.Variable("data")
    val fc1 = Symbol.FullyConnected("fc1")(input)(Map("num_hidden" -> 2))
    val test = Symbol.elemwise_add("sum")(input, fc1)()
    val act1 = Symbol.Activation("sig1")(fc1)(Map("act_type" -> "sigmoid"))
    val fc2 = Symbol.FullyConnected("fc2")(act1)(Map("num_hidden" -> 1))
    val out = Symbol.LogisticRegressionOutput("sigout")(fc2)()
    
    val x = NDArray.array(Array(0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f), Shape(4,2), ctx)
    val y = NDArray.array(Array(0.0f, 1.0f, 1.0f, 0.0f), Shape(4), ctx)
    val trainData = Array(x)
    val trainLabels = Array(y)
    val trainDataIter = new NDArrayIter(trainData, trainLabels, dataBatchSize=4, labelName="sigout_label")
    println(trainDataIter.provideData)
    println(trainDataIter.provideLabel)
    
    val module = new Module(out, dataNames=Array("data"), labelNames=Array("sigout_label"))
    val trainDataDescs = trainDataIter.provideData.map { case(k, v) => new DataDesc(k, v, layout="NC") }.toIndexedSeq
    val trainLabelDescs = trainDataIter.provideLabel.map { case(k, v) => new DataDesc(k, v, layout="NC") }.toIndexedSeq
    module.bind(trainDataDescs, Some(trainLabelDescs), forTraining=true, inputsNeedGrad=true)
    module.initParams(new Xavier())
    module.initOptimizer(optimizer=new RMSProp(learningRate=0.1f))
    module.fit(trainDataIter, numEpoch=1)
    println(module.getInputGrads().head.head.toArray.mkString(" "))
    module.forward(trainDataIter.next())
    println(module.getOutputs().head.head.toArray.mkString(" "))
    val mae = new MAE()
    module.score(new NDArrayIter(trainData, trainLabels, dataBatchSize=4), mae)
    val (_, scores) = mae.get
    println(scores.mkString(" "))
    
//    val ff = FeedForward.newBuilder(out)
//      .setContext(ctx)
//      .setBatchSize(4)
//      .setNumEpoch(1000)
//      .setOptimizer(new SGD(learningRate = 0.1f, momentum=0.9f))
//      .setTrainData(trainDataIter)
//      .setEvalData(new NDArrayIter(trainData, trainLabels, dataBatchSize=4))
//      .setInitializer(new Xavier())
//      .build();
//    println(ff.predict(trainDataIter, 4).head.toArray.mkString(" "))
  }
  
  def testBucketing {
    def symGen(key: AnyRef): (Symbol, IndexedSeq[String], IndexedSeq[String]) = {
      val len = key.asInstanceOf[Int]
      val syms = Array.ofDim[Symbol](len)
      val names = Array.ofDim[String](len)
      for (i <- 1 to len) {
        names(i-1) = "x_" + i
        syms(i-1) = Symbol.Variable(names(i-1))
      }
      val sum = Symbol.ElementWiseSum("")(syms:_*)()
      val sum2 = sum * 2
      val cc = Symbol.Concat()(sum, sum2)(Map("dim" -> 0))
      (cc, names, Array[String]())
    }
    
    val ctx = Context.cpu(0)
    val module = new BucketingModule(symGen, 1.asInstanceOf[AnyRef])
    module.bind(Array(DataDesc("x_1", Shape(1,2))))
    module.initParams(new Xavier())
    module.switchBucket(2.asInstanceOf[AnyRef], Array(DataDesc("x_1", Shape(1,2), layout="NC"), DataDesc("x_2", Shape(1,2), layout="NC")))
    val dataMapping = ListMap[String, Shape]("x_1" -> Shape(1,2), "x_2" -> Shape(1,2))
    val labelMapping = ListMap[String, Shape]()
    val batch = new DataBatch(Array(NDArray.array(Array(1, 1), Shape(1,2), ctx), NDArray.array(Array(1, 1), Shape(1,2), ctx)), Array[NDArray](), Array(0), 0, 2.asInstanceOf[AnyRef], dataMapping, labelMapping)
    module.forward(batch, isTrain=Some(true))
    println(module.getOutputs().head.head.shape)
    
  }
}
