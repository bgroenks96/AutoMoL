package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._
import botkop.numsca.Tensor
import botkop.{numsca => ns}

import scala.collection.mutable.Set
import org.nd4j.linalg.api.ndarray.INDArray

abstract class BaseFeature extends Feature {
  def apply(state: ProblemState, action: Action) = BaseFeature.applyFunc(eval)(state, action)
  
  def eval(graph: ProblemGraph, action: Action): Double
}

object BaseFeature {
  def applyFunc(evalFunc: (ProblemGraph, Action) => Double): (ProblemState, Action) => Double = {
    (state, action) => state match {
      case WorkingState(graph, _) => evalFunc(graph, action)
      case _ => 0.0
    }
  }
}

object Features {
  def introRuleFilter: Feature = {
    def relevance(graph: ProblemGraph, action: Action) = graph.goal match {
      case AtomicNode(a) => if (action.rule.yields(AtomicSentence(a))) 1.0 else -1.0
      case BinaryNode(s) => if (action.rule.yields(s)) 1.0 else -1.0
      case UnaryNode(s) => if (action.rule.yields(s)) 1.0 else -1.0
      case QuantifierNode(s) => if (action.rule.yields(s)) 1.0 else -1.0
      case AbsurdityNode => if (action.rule.yields(Absurdity)) 1.0 else -1.0
      case _ => 0.0
    }
    BaseFeature.applyFunc(relevance)
  }
  
  def ruleOrdering: Feature = {
    val ruleCount = 7
    def order(graph: ProblemGraph, action: Action) = action.rule match {
      case AndElimination => 1.0
      case OrElimination => 1.0 - 1*1.0/ruleCount
      case NegationElimination => 1.0 - 2*1.0/ruleCount
      case AndIntroduction => 1.0 - 3*1.0/ruleCount
      case IfIntroduction => 1.0 - 4*1.0/ruleCount
      case NegationIntroduction => 1.0 - 5*1.0/ruleCount
      case IfElimination => 1.0 - 6*1.0/ruleCount
      case OrIntroduction => 1.0 - 7*1.0/ruleCount
    }
    BaseFeature.applyFunc(order)
  }
  
  def accessibility: Feature = {
    def accessible(graph: ProblemGraph, action: Action) = graph.goal match {
      case AtomicNode(atom)
        if (toSentences(graph.assumptions:_*).exists(s => s.accessible(atom))) => 1.0
      case AtomicNode(_) => -1.0
      case _ => 1.0
    }
    BaseFeature.applyFunc(accessible)
  }
  
  def similarityScore(dist: Int): Feature = {
    def walk(graph: ProblemGraph, node: GraphNode, dist: Int, visited: Set[GraphNode] = Set()): Tensor = dist match {
      case d if d == 0 => graph.encodings(node)
      case d =>
        visited.add(node)
        val s = (graph.graph.adjIn(node) ++ graph.graph.adjOut(node))
          .filter(n => !visited.contains(n))
          .map(n => walk(graph, n, d - 1, visited))
          .reduce((t1, t2) => t1 + t2)
        s + graph.encodings(node)
    }
    def score(dist: Int)(graph: ProblemGraph, action: Action) = action.major match {
      case Some(s) =>
        find(s, graph.assumptions) match {
          case Some(node) =>
            val nx = graph.encodings(node) + walk(graph, node, dist, Set(node))
            val cx = graph.encodings(graph.goal) + walk(graph, graph.goal, dist, Set(graph.goal))
            (ns.dot(nx, cx.T) / (ns.dot(nx, nx.T)*ns.dot(cx, cx.T))).squeeze()
          case None => 0.0
        }
      case None => 0.0
    }
    BaseFeature.applyFunc(score(dist))
  }
  
  def shortestPathToGoal: Feature = {
    import scala.collection.mutable.Map
    def shortestPath(graph: ProblemGraph, action: Action) = action.major match {
      case Some(s) => find(s, graph.assumptions) match {
        case Some(n) =>
          // Dijkstra's shortest-path algorithm
          val distMap = Map[GraphNode, Int]()
          val unvisited = Set[GraphNode]()
          graph.graph.nodes.foreach {
            v =>
              distMap(v) = Int.MaxValue
              unvisited.add(v)
          }
          distMap(n) = 0
          while (!unvisited.isEmpty) {
            val min = unvisited.min[GraphNode](Ordering.by(v => distMap(v)))
            unvisited.remove(min)
            graph.graph.adjOut(min).foreach {
              v =>
                val d = distMap(min) + 1
                if (d < distMap(v)) {
                  distMap(v) = d
                }
            }
          }
          1.0 / distMap(graph.goal)
        case None => 0.0
      }
      case None => 0.0
    }
    BaseFeature.applyFunc(shortestPath)
  }
  
  private def toSentences(nodes: GraphNode*) = nodes.map {
    n => n match {
      case AtomicNode(atom) => AtomicSentence(atom)
      case BinaryNode(s) => s
      case UnaryNode(s) => s
      case QuantifierNode(s) => s
      case AbsurdityNode => Absurdity
      case _ => ???
    }
  }
  
  private def find(s: Sentence, nodes: Seq[GraphNode]) = nodes.find {
    n => n match {
      case AtomicNode(atom) => AtomicSentence(atom).matches(s)
      case BinaryNode(ns) => ns.matches(s)
      case UnaryNode(ns) => ns.matches(s)
      case QuantifierNode(ns) => ns.matches(s)
      case AbsurdityNode => s == Absurdity
      case _ => ???
    }
  }
}
