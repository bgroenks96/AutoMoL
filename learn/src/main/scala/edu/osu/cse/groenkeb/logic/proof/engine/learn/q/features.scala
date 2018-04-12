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
  
  def concMatching: Feature = {
    def sentMatch(graph: ProblemGraph, action: Action) = (graph.goal, action.major) match {
      case (AtomicNode(_), Some(sentence)) => sentence match { case AtomicSentence(_) => 1.0; case _ => 0.0 }
      case (BinaryNode(And(_,_)), Some(sentence)) => sentence match { case And(_,_) => 1.0; case _ => 0.0 }
      case (BinaryNode(Or(_,_)), Some(sentence)) => sentence match { case Or(_,_) => 1.0; case _ => 0.0 }
      case (BinaryNode(If(_,_)), Some(sentence)) => sentence match { case If(_,_) => 1.0; case _ => 0.0 }
      case (UnaryNode(Not(_)), Some(sentence)) => sentence match { case Not(_) => 1.0; case _ => 0.0 }
      case (QuantifierNode(ForAll(_,_)), Some(sentence)) => sentence match { case ForAll(_,_) => 1.0; case _ => 0.0 }
      case (QuantifierNode(Exists(_,_)), Some(sentence)) => sentence match { case Exists(_,_) => 1.0; case _ => 0.0 }
      case _ => 0.0
    }
    BaseFeature.applyFunc(sentMatch)
  }
}
