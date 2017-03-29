package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

case class NaiveProofStrategy() extends ProofStrategy {
  /**
   * Returns a sequence of rules in order of strategic precedence given the current proof context. 
   * The returned sequence must be a subset of the rule set in the given context.
   */
  def rules(implicit context: ProofContext): RuleSet = context.rules
  
  /**
   * Returns a sequence of premises in order of strategic precedence given the current proof context.
   * The returned sequence must be a subset of the premises in the given context.
   */
  def premises(implicit context: ProofContext): Seq[Premise] = context.premises
  
  /**
   * Determines an appropriate search result for the given proof in the current context.
   */
  def decide(proof: Proof)(implicit context: ProofContext): ProofResult = proof match {
    case CompleteProof(c, p) if c.sentence.matches(context.goal) => Success(CompleteProof(c, p), Cut())
    case CompleteProof(c, p) => Success(CompleteProof(c, p), Continue())
    case NullProof(p) => Failure(NullProof(p), Continue())
  }
}
