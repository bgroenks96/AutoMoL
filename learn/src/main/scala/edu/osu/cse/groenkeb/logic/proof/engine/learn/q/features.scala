package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic._

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
  def ruleMatching(rules: RuleSet): Seq[Feature] = {
    def ruleMatch(rule: Rule)(graph: ProblemGraph, action: Action) = if (action.rule == rule) 1.0 else 0.0
    rules.rules.map[Feature, Seq[Feature]](r => BaseFeature.applyFunc(ruleMatch(r)))
  }
  
  def ruleRelevance: Feature = {
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
}
