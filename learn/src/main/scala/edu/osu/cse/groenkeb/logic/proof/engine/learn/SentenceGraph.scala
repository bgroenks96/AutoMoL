package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.Premise
import edu.osu.cse.groenkeb.logic.proof.ProofContext

import scala.collection.Seq
import scala.collection.mutable.Map

object SentenceGraph {
  def apply(sentence: Sentence, parent: Option[GraphNode] = None): (SentenceGraph, GraphNode) = {
    val adjMap = Map[GraphNode, Adjacency]()
    val node = build(sentence, parent, adjMap)
    (SentenceGraph(adjMap), node)
  }
  
  private[learn] def build(sentence: Sentence, parent: Option[GraphNode] = None, adjMap: Map[GraphNode, Adjacency]):
    GraphNode = sentence match {
    case Absurdity =>
      val node = AbsurdityNode
      adjMap.put(node, createAdj(node, Nil, parent))
      node
    case AtomicSentence(atom) =>
      val node = AtomicNode(atom)
      val pred = PredicateNode(atom.predicate)
      adjMap.put(pred, createAdj(pred, Nil, parent))
      val terms = atom.terms.map(t => VarNode(t))
      terms.foreach(v => adjMap.put(v, createAdj(v, Nil, parent)))
      adjMap.put(node, createAdj(node, pred +: terms, parent))
      node
    case s@BinarySentence(left, right, _) =>
      val node = BinaryNode(s)
      val leftNode = build(left, Some(node), adjMap)
      val rightNode = build(right, Some(node), adjMap)
      adjMap.put(node, createAdj(node, Seq(leftNode, rightNode), parent))
      node
    case s@UnarySentence(operand, _) =>
      val node = UnaryNode(s)
      val subNode = build(operand, Some(node), adjMap)
      adjMap.put(node, createAdj(node, Seq(subNode), parent))
      node
    case s@QuantifiedSentence(sentence, quant) =>
      val node = QuantifierNode(s)
      val termNode = VarNode(quant.term)
      val exprNode = build(sentence, Some(node), adjMap)
      adjMap.put(node, createAdj(node, Seq(termNode, exprNode), parent))
      node
    case _ => ???
  }
  
  private def createAdj(node: GraphNode, children: Seq[GraphNode], parent: Option[GraphNode]): Adjacency =
    Adjacency(parent.map(n => Seq(n)).getOrElse(Nil), children)
}

final case class SentenceGraph(adj: Map[GraphNode, Adjacency]) {
  def nodes = adj.keys.seq
  
  def size = nodes.size
  
  def has(node: GraphNode) = adj.contains(node)
  
  def adjIn(node: GraphNode) = adj.getOrElse(node, Adjacency()).in
  
  def adjOut(node: GraphNode) = adj.getOrElse(node, Adjacency()).out
  
  def neighbors(node: GraphNode) = {
    val adj = this.adj.getOrElse(node, Adjacency())
    (adj.in ++ adj.out).distinct
  }
  
  def degree(node: GraphNode) = adj.getOrElse(node, Adjacency()).degree
  
  def ++(graph: SentenceGraph) =
    SentenceGraph(graph.adj ++ adj.map{ case (k,v) => k -> (v ++ graph.adj.getOrElse(k, Adjacency())) })
    
  override def toString =
    adj.map {case (n, s) => n.toString() + "->[" + s + "]"}.mkString("{", "; ", "}")
}

final case class Adjacency(in: Seq[GraphNode] = Nil, out: Seq[GraphNode] = Nil) {
  def ++(adj: Adjacency) = Adjacency(merge(in, adj.in), merge(out, adj.out))
  
  def degreeIn = in.length
  
  def degreeOut = out.length
  
  def degree = degreeIn + degreeOut
  
  // merges s2 into s1, ignoring nodes in s2 duplicating those in s1, but preserving duplicates
  // that are independent to each sequence
  private def merge(s1: Seq[GraphNode], s2: Seq[GraphNode]) = s1 ++ s2.filterNot { s => s1.contains(s) }
  
  override def toString = s"in: [${in.mkString(",")}] out: [${out.mkString(",")}]"
}

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
final case class AtomicNode(atom: Atom) extends GraphNode {
  override def toString = s"atom:${atom.toString()}"
}
final case class PredicateNode(pred: Predicate) extends GraphNode {
  override def toString = s"pred:${pred.toString()}"
}
final case class VarNode(term: Term) extends GraphNode {
  override def toString = term.toString()
}
final case object AbsurdityNode extends GraphNode {
  override def toString = "!"
}
