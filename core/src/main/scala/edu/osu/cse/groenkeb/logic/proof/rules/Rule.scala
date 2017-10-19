package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.Proof

trait Rule {
  /**
   * True if this rule accepts the given proof as a MAJOR premise, false otherwise.
   */
  def major(proof: Proof): Boolean

  /**
   * True if this rule yields the given sentence as a conclusion, false otherwise.
   */
  def yields(sentence: Sentence): Boolean

  /**
   * Try to infer a given sentence from the given set of proven premises.
   */
  def infer(conclusion: Sentence)(from: RuleArgs): InferenceResult
}
