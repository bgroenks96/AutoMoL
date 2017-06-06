package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.Discharge
import edu.osu.cse.groenkeb.logic.Sentence

sealed abstract class Proof(val conclusion: Option[Conclusion], val premises: Set[Premise])
case class CompleteProof(conc: Conclusion, prems: Set[Premise]) extends Proof(Option.apply(conc), prems)
case class NullProof() extends Proof(Option.empty[Conclusion], Set())

object CompleteProof {
  /**
   * Shorthand constructor that generates the Conclusion argument from the given values.
   */
  def apply(conc: Sentence, crule: Rule, cargs: RuleArgs, proofPremises: Set[Premise]): CompleteProof = {
    CompleteProof(Conclusion(conc, crule, cargs), proofPremises)
  }
}
