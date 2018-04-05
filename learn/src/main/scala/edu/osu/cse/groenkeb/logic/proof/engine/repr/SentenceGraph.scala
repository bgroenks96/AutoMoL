package edu.osu.cse.groenkeb.logic.proof.engine.repr

import edu.osu.cse.groenkeb.logic._
import scala.annotation.tailrec

sealed abstract class GraphNode
final case class QuantifierNode(sentence: QuantifiedSentence) extends GraphNode {
  override def toString = sentence.toString()
}
final case class BinaryNode(sentence: BinarySentence) extends GraphNode {
  override def toString = sentence.toString()
}
final case class UnaryNode(sentence: UnarySentence) extends GraphNode {
  override def toString = sentence.toString()
}
final case class AtomicNode(sentence: AtomicSentence) extends GraphNode {
  override def toString = sentence.toString()
}
final case class VarNode(term: Term) extends GraphNode {
  override def toString = term.toString()
}

object SentenceGraph {
  type AdjacencyList = Map[GraphNode, Set[GraphNode]]
  
  def apply(sentence: Sentence) = build(sentence, None)
  
  private def build(sentence: Sentence, parent: Option[GraphNode]): SentenceGraph = sentence match {
    case s@AtomicSentence(atom) =>
      val node = AtomicNode(s)
      val children = atom.terms.map(t => VarNode(t))
      SentenceGraph(addParent(node, parent) ++ newAdj(node, children) ++ children.map(n => n -> Set[GraphNode]()).toMap)
    case s@BinarySentence(left, right, _) =>
      val node = BinaryNode(s)
      SentenceGraph(addParent(node, parent)) ++ build(left, Some(node)) ++ build(right, Some(node))
    case s@UnarySentence(operand, _) =>
      val node = UnaryNode(s)
      SentenceGraph(addParent(node, parent)) ++ build(operand, Some(node))
    case s@QuantifiedSentence(sentence, quant) =>
      val node = QuantifierNode(s)
      SentenceGraph(addParent(node, parent) ++ newAdj(node, Seq(VarNode(quant.term)))) ++ build(sentence, Some(node))
    case _ => ???
  }
  
  private def addParent(child: GraphNode, parent: Option[GraphNode]): AdjacencyList = parent match {
    case Some(node) => newAdj(node, Seq(child))
    case None => Map()
  }
  
  private def newAdj[T <: GraphNode](node: GraphNode, children: Seq[T]): AdjacencyList = Map(node -> children.toSet[GraphNode])
}

final case class SentenceGraph(adj: SentenceGraph.AdjacencyList) {
  def nodes = adj.keys.seq
  
  def size = nodes.size
  
  def ++(graph: SentenceGraph) =
    SentenceGraph(graph.adj ++ adj.map{ case (k,v) => k -> (v ++ graph.adj.getOrElse(k, Set())) })
    
  override def toString =
    adj.map {case (n, s) => n.toString() + "->[" + s.mkString(",") + "]"}.mkString("{", "; ", "}")
}
