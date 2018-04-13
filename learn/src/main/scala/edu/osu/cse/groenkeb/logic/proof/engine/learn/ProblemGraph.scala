package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import botkop.{numsca => ns}
import botkop.numsca.Tensor

object ProblemGraph {
  val encodingDims = 11  

  def apply(context: ProofContext): ProblemGraph = {
    val rootNode = RootNode(context.goal, context.available.toSeq.map(p => p.sentence))
    val (goalGraph, goalNode) = SentenceGraph(context.goal, Some(rootNode))
    val (premGraphs, premNodes) = context.available.toSeq.map(p => SentenceGraph(p.sentence, Some(rootNode))).unzip
    val rootGraph = SentenceGraph(Map[GraphNode, Adjacency](rootNode -> Adjacency(Nil, goalNode +: premNodes)))
    val mergedGraph = rootGraph ++ ((goalGraph +: premGraphs).reduce[SentenceGraph]{ case (g1, g2) => g1 ++ g2 })
    ProblemGraph(mergedGraph, goalNode, premNodes, rootNode)
  }
  
  def encode(node: GraphNode): Tensor = node match {
    case RootNode(_,_) => onehot(0)
    case AtomicNode(_) => onehot(1)
    case BinaryNode(And(_, _)) => onehot(2)
    case BinaryNode(Or(_, _)) => onehot(3)
    case BinaryNode(If(_, _)) => onehot(4)
    case UnaryNode(Not(_)) => onehot(5)
    case QuantifierNode(ForAll(_,_)) => onehot(6)
    case QuantifierNode(Exists(_,_)) => onehot(7)
    case VarNode(_) => onehot(8)
    case PredicateNode(_) => onehot(9)
    case AbsurdityNode => onehot(10)
    case _ => ???
  }
  
  private def onehot(ind: Int): Tensor = {
    require(ind >= 0)
    require(ind < encodingDims)
    val res = ns.zeros(encodingDims)
    res(ind) := 1.0
    res
  }
}

case class ProblemGraph(graph: SentenceGraph, goal: GraphNode, assumptions: Seq[GraphNode], root: RootNode) {
  //val encodings = initEncodings(root)
  
  private def initEncodings(node: GraphNode, sign: Int = 1): Map[GraphNode, Tensor] = {
    require(sign == 1 || sign == -1)
    val outEdges = graph.adjOut(node)
    outEdges.zipWithIndex.collect{ case (n, i) => {
       initEncodings(n, signOf(node, i, outEdges.size))
    }}.fold(Map(node -> ProblemGraph.encode(node) * sign))((m1, m2) => m1 ++ m2)
  }
  
  private def signOf(node: GraphNode, ind: Int, outCount: Int): Int = node match {
    case RootNode(_,_) if ind < outCount - 1 => -1
    case BinaryNode(If(_, _)) if ind == 0 => -1
    case UnaryNode(Not(_)) => -1
    case _ => 1
  }
}
