package edu.osu.cse.groenkeb.logic.proof.engine.learn.util

import ml.dmlc.mxnet._
import ml.dmlc.mxnet.io.NDArrayIter

object MxUtil {
  def array(vals: Float*)(implicit ctx: Context) = NDArray.array(vals.toArray, Shape(vals.length), ctx)
  
  def array1D(vals: Float*)(implicit ctx: Context) = NDArray.array(vals.toArray, Shape(1, vals.length), ctx)
  
  def onehot(ind: Int, out: NDArray)(implicit ctx: Context) = {
    val indices = array(ind)
    try {
      NDArray.onehotEncode(indices, out)
    } finally {
      indices.dispose()
    }
  }
  
  def iter(x: NDArray*) = new NDArrayIter(x.toArray[NDArray], dataBatchSize=1)
  
  def bkt[T](id: T): AnyRef = id.asInstanceOf[AnyRef]
}
