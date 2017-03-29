package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.Proof

trait Rule {
  /**
   * The "parity" of this rule; must be either elimination, introduction, or none.
   */
  def parity: RuleParity

  /**
   * True if this rule accepts the given proof as a premise, false otherwise.
   */
  def accepts(proof: Proof): Boolean

  /**
   * True if this rule yields the given sentence as a conclusion, false otherwise.
   */
  def yields(sentence: Sentence): Boolean

  /**
   * Try to infer a given sentence from the given set of proven premises.
   */
  def infer(conclusion: Sentence)(from: RuleArgs): InferenceResult
}
