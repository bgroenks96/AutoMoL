package edu.osu.cse.groenkeb.logic.proof.engine.learn.repr

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.engine.learn._
import edu.osu.cse.groenkeb.logic.proof.engine.learn.util.MxUtil

import ml.dmlc.mxnet._
import scala.collection.mutable.ArrayStack

object NodeEmbedding {
  val embeddingSize = 10
}

final class NodeEmbedding(val node: GraphNode, val nodeDegree: Int) {
  private implicit val ctx = Context.defaultCtx
  
  private val values: ArrayStack[NDArray] = ArrayStack(initialEmbedding)
  
  private val embeddingSize = NodeEmbedding.embeddingSize
  
  def currentValue = values.last
  
  def push(newValue: NDArray) {
    values.push(newValue)
  }
  
  def pop = values.pop()
  
  def dispose {
    values.foreach(a => a.dispose())
    values.clear()
  }
  
  private def initialEmbedding = node match {
    case AtomicNode(_) => MxUtil.onehot(1, NDArray.zeros(1, embeddingSize))
    case BinaryNode(And(_, _)) => MxUtil.onehot(2, NDArray.zeros(1, embeddingSize))
    case BinaryNode(Or(_, _)) => MxUtil.onehot(3, NDArray.zeros(1, embeddingSize))
    case BinaryNode(If(_, _)) => MxUtil.onehot(4, NDArray.zeros(1, embeddingSize))
    case UnaryNode(Not(_)) => MxUtil.onehot(5, NDArray.zeros(1, embeddingSize))
    case QuantifierNode(ForAll(_,_)) => MxUtil.onehot(6, NDArray.zeros(1, embeddingSize))
    case QuantifierNode(Exists(_,_)) => MxUtil.onehot(7, NDArray.zeros(1, embeddingSize))
    case VarNode(_) => MxUtil.onehot(8, NDArray.zeros(1, embeddingSize))
    case PredicateNode(_) => MxUtil.onehot(9, NDArray.zeros(1, embeddingSize))
    case _ => ???
  }
}