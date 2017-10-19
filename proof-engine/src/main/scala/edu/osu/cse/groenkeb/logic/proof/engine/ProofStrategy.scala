package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.proof.Premise
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

import scala.collection.immutable.Seq
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.Premise

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
   * Returns an appropriate final ProofResult for the given ProofResult, according to this strategy.
   * This allows the ProofStrategy to make any necessary changes to the status of the proof result (like
   * Cut where the search engine would have defaulted to Continue, etc).
   */
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult
}
