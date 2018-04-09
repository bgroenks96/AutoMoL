package edu.osu.cse.groenkeb.logic.engine.learn.mxnet

import ml.dmlc.mxnet._
import ml.dmlc.mxnet.module.Module
import ml.dmlc.mxnet.io.NDArrayIter
import ml.dmlc.mxnet.EvalMetric
import ml.dmlc.mxnet.optimizer.Adam
import ml.dmlc.mxnet.optimizer.SGD

object MxNetPlayground {
  def main(args: Array[String]) = {
    testMultiLayerPerceptronXOR()
  }
  
  def testMultiLayerPerceptronXOR() = {
    val ctx = Context.cpu(0)
    val input = Symbol.Variable("data")
    val fc1 = Symbol.FullyConnected("fc1")(input)(Map("num_hidden" -> 2))
    val act1 = Symbol.Activation("sig1")(fc1)(Map("act_type" -> "sigmoid"))
    val fc2 = Symbol.FullyConnected("fc2")(act1)(Map("num_hidden" -> 1))
    val out = Symbol.LogisticRegressionOutput("sigout")(fc2)()
    
    val trainData = Array(NDArray.array(Array(0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f, 1.0f, 1.0f), Shape(4,2), ctx))
    val trainLabels = Array(NDArray.array(Array(0.0f, 1.0f, 1.0f, 0.0f), Shape(4), ctx))
    val trainDataIter = new NDArrayIter(trainData, trainLabels, dataBatchSize=4, labelName="sigout_label")
    println(trainDataIter.provideData)
    println(trainDataIter.provideLabel)
    
    val module = new Module(out, dataNames=Array("data"), labelNames=Array("sigout_label"))
    val trainDataDescs = trainDataIter.provideData.map { case(k, v) => new DataDesc(k, v, layout="NC") }.toIndexedSeq
    val trainLabelDescs = trainDataIter.provideLabel.map { case(k, v) => new DataDesc(k, v, layout="NC") }.toIndexedSeq
    module.bind(trainDataDescs, Some(trainLabelDescs), forTraining=true)
    module.initParams(new Xavier())
    module.initOptimizer(optimizer=new Adam())
    module.fit(trainDataIter, numEpoch=5000)
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
}
