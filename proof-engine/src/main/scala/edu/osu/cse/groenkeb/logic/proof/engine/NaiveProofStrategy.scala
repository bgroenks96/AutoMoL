package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.proof.Premise
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import scala.collection.immutable.Seq
import edu.osu.cse.groenkeb.logic.proof.ProofContext

case class NaiveProofStrategy() extends ProofStrategy {
  def rules(implicit context: ProofContext): RuleSet = context.rules
  
  def premises(implicit context: ProofContext): Seq[Premise] = Seq(context.premises.toSeq:_*)
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
}
