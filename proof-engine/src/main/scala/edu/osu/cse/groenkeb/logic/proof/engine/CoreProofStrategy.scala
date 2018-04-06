package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action

case class CoreProofStrategy() extends ProofStrategy {
  private implicit val ruleOrdering = Ordering[Int].on[Rule]((r: Rule) => ruleOrdinal(r))
  
  def actions(implicit context: ProofContext) = generateActions
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
  
  private def generateActions(implicit context: ProofContext) = {
    for {
      rule <- filterRules.sorted
      action <- context.available.filter { p => rule.major(p.sentence) } match {
        case avail if avail.isEmpty => Seq(Action(rule))
        case avail => avail.map { p => Action(rule, Some(p.sentence)) }.toSeq
      }
    } yield action
  }
  
  private def filterRules(implicit context: ProofContext): Seq[Rule] = context.rules.collect {
    case r@AndElimination => r
    case r@OrElimination => r
    case r@NegationElimination /*if context.goal is Absurdity*/ => r
    case r@AndIntroduction if context.goal has And => r
    case r@IfIntroduction if context.goal has If => r
    case r@NegationIntroduction if context.goal has Not => r
    case r@IfElimination if (context.goal.isAtomic) or (context.goal has Or) => r
    case r@OrIntroduction if context.goal has Or => r
  }
  
  private def ruleOrdinal(rule: Rule) = rule match {
    case AndElimination => 0
    case OrElimination => 1
    case NegationElimination => 2
    case AndIntroduction => 3
    case IfIntroduction => 4
    case NegationIntroduction => 5
    case IfElimination => 6
    case OrIntroduction => 7
  }
}