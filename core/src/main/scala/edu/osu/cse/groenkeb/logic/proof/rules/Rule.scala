package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._

trait Rule {
  /**
   * True if this rule can accept the given sentence as a major premise for inference, false otherwise.
   * Rules that do not define a major premise should return false on all inputs.
   */
  def major(sentence: Sentence): Boolean
  
  /**
   * True if this rule yields the given sentence as a conclusion, false otherwise.
   */
  def yields(sentence: Sentence): Boolean

  /**
   * Returns the RuleParams necessary for inference given the conclusion ("goal") of the current ProofContext and the
   * optional major premise of the rule. If this rule does not define a major premise, the supplied argument
   * should be None. Implementations should return None for all conclusion/major patterns that are not defined
   * the Rule.
   */
  def params(major: Option[Sentence] = None)(implicit context: ProofContext): Option[RuleParams]
  
  /**
   * Returns a Proof with the current goal of the given ProofContext as the conclusion and the arguments supplied
   * in 'args' as the premises, or None if the given arguments do not satisfy this Rule's parameters for inference.
   */
  def infer(args: RuleArgs)(implicit context: ProofContext): Option[Proof]
}
