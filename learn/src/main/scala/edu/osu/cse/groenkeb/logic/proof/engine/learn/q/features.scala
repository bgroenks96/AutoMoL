package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._

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
      case AtomicNode(a) => if (action.rule.yields(AtomicSentence(a))) 1.0 else 0.0
      case BinaryNode(s) => if (action.rule.yields(s)) 1.0 else 0.0
      case UnaryNode(s) => if (action.rule.yields(s)) 1.0 else 0.0
      case QuantifierNode(s) => if (action.rule.yields(s)) 1.0 else 0.0
      case AbsurdityNode => if (action.rule.yields(Absurdity)) 1.0 else 0.0
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
      case AtomicNode(atom) =>
        if (toSentences(graph.assumptions:_*).exists(s => s.accessible(atom))) 0.0 else 1.0
      case _ => 0.0
    }
    BaseFeature.applyFunc(accessible)
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
}
