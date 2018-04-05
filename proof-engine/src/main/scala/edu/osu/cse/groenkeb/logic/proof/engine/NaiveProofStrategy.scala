package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import scala.collection.immutable.Seq

case class NaiveProofStrategy() extends ProofStrategy {

  def actions(implicit context: ProofContext) =
    for {
      rule <- context.rules
      action <- context.available.filter { p => rule.major(p.sentence) } match {
        case avail if avail.isEmpty => Seq(Action(rule))
        case avail => avail.map { p => Action(rule, Some(p.sentence)) }.toSeq
        case _ => edu.osu.cse.groenkeb.logic.proof.rules.core.AndElimination; null
      }
    } yield action
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
}
