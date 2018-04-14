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
  
  def eval(graph: WorkingState, action: Action): Double
}

object BaseFeature {
  def applyFunc(evalFunc: (WorkingState, Action) => Double): (ProblemState, Action) => Double = {
    (state, action) => state match {
      case s@WorkingState(_, _, _) => evalFunc(s, action)
      case _ => 0.0
    }
  }
}

object Features {
  def basicRuleFilter: Feature = {
    def relevance(state: WorkingState, action: Action) = action.rule match {
      case r@AndElimination => 1.0
      case r@OrElimination => 1.0
      case r@NegationElimination /*if state.goal is Absurdity*/ => 1.0
      case r@AndIntroduction if state.goal has And => 1.0
      case r@IfIntroduction if state.goal has If => 1.0
      case r@NegationIntroduction if state.goal has Not => 1.0
      case r@IfElimination if (state.goal.isAtomic) or (state.goal has Or) => 1.0
      case r@OrIntroduction if state.goal has Or => 1.0
      case _ => -1.0
    }
    BaseFeature.applyFunc(relevance)
  }
  
  def ruleOrdering: Feature = {
    val ruleCount = 7
    def order(state: WorkingState, action: Action) = action.rule match {
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
  
  def atomicAccessibility: Feature = {
    def accessible(state: WorkingState, action: Action) = state.goal match {
      case a:AtomicSentence if state.assumptions.forall(p => !p.sentence.accessible(a)) => -1.0
      case a:AtomicSentence => 1.0
      case _ => 0.0
    }
    BaseFeature.applyFunc(accessible)
  }
  
//  def similarityScore(dist: Int): Feature = {
//    def walk(graph: ProblemGraph, node: GraphNode, dist: Int, visited: Set[GraphNode] = Set()): Tensor = dist match {
//      case d if d == 0 => graph.encodings(node)
//      case d =>
//        visited.add(node)
//        val s = (graph.graph.adjIn(node) ++ graph.graph.adjOut(node))
//          .filter(n => !visited.contains(n))
//          .map(n => walk(graph, n, d - 1, visited))
//          .fold(ns.zeros(ProblemGraph.encodingDims))((t1, t2) => t1 + t2)
//        s + graph.encodings(node)
//    }
//    def score(dist: Int)(state: WorkingState, action: Action) = action.major match {
//      case Some(s) =>
//        val graph = state.graph
//        find(s, graph.assumptions) match {
//          case Some(node) =>
//            val nx = graph.encodings(node) + walk(graph, node, dist, Set(node))
//            val cx = graph.encodings(graph.goal) + walk(graph, graph.goal, dist, Set(graph.goal))
//            (ns.dot(nx, cx.T) / (ns.dot(nx, nx.T)*ns.dot(cx, cx.T))).squeeze()
//          case None => 0.0
//        }
//      case None => 0.0
//    }
//    BaseFeature.applyFunc(score(dist))
//  }
  
  def shortestPathToGoal: Feature = {
    import scala.collection.mutable.Map
    def shortestPath(state: WorkingState, action: Action) = action.major match {
      case Some(s) => find(s, state.graph.assumptions) match {
        case Some(n) =>
          val ngraph = state.graph.graph
          // Dijkstra's shortest-path algorithm
          val distMap = Map[GraphNode, Int]()
          val unvisited = Set[GraphNode]()
          ngraph.nodes.foreach {
            v =>
              distMap(v) = Int.MaxValue
              unvisited.add(v)
          }
          distMap(n) = 0
          while (!unvisited.isEmpty) {
            val min = unvisited.min[GraphNode](Ordering.by(v => distMap(v)))
            unvisited.remove(min)
           (ngraph.adjIn(min) ++ ngraph.adjOut(min)).foreach {
              v =>
                val d = distMap(min) + 1
                if (d < distMap(v)) {
                  distMap(v) = d
                }
            }
          }
          1.0 / (distMap(state.graph.goal) + 1)
        case None => 0.0
      }
      case None => 0.0
    }
    BaseFeature.applyFunc(shortestPath)
  }
  
  def majorComplexityScore: Feature = {
    def majorComplexity(state: WorkingState, action: Action) = action.major match {
      case Some(major) if state.assumptions.exists(p => p.matches(major)) =>
        val prems = state.assumptions.map(p => p.sentence).toSeq
        val goalComplexity = state.goal.complexity
        val premComplexity = prems.map(s => s.complexity)
        val majorPremIndex = prems.indexOf(major)
        val max = (goalComplexity +: premComplexity).max
        1.0 - premComplexity(majorPremIndex) / max.toDouble
      case _ => 0.0
    }
    BaseFeature.applyFunc(majorComplexity)
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
