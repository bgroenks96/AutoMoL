package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.Sentence

sealed case class Proof(val conclusion: Conclusion, val premises: Set[Premise])

object Proof {
  /**
   * Shorthand constructor that generates the Conclusion argument from the given values.
   */
  def apply(conc: Sentence, crule: Rule, cargs: RuleArgs, proofPremises: Set[Premise]): Proof = {
    Proof(Conclusion(conc, crule, cargs), proofPremises)
  }
}
