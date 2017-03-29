package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

trait ProofStrategy {
  /**
   * Returns a sequence of rules in order of strategic precedence given the current proof context. 
   * The returned sequence must be a subset of the rule set in the given context.
   */
  def rules(implicit context: ProofContext): RuleSet
  
  /**
   * Returns a sequence of premises in order of strategic precedence given the current proof context.
   * The returned sequence must be a subset of the premises in the given context.
   */
  def premises(implicit context: ProofContext): Seq[Premise]
  
  /**
   * Determines an appropriate search result for the given proof in the current context.
   */
  def decide(proof: Proof)(implicit context: ProofContext): ProofResult
}
