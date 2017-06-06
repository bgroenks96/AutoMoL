package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

import scala.collection.immutable.Seq

case class NaiveProofStrategy() extends ProofStrategy {
  def rules(implicit context: ProofContext): RuleSet = context.rules
  
  def premises(implicit context: ProofContext): Seq[Premise] = Seq(context.premises.toSeq:_*)
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
}
