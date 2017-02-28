package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.Discharge

sealed abstract class Proof(val conclusion: Option[Conclusion], val premises: Seq[Premise])
case class CompleteProof(conc: Conclusion, prems: Seq[Premise]) extends Proof(Option.apply(conc), prems)
case class NullProof(prems: Seq[Premise]) extends Proof(Option.empty[Conclusion], prems)

object CompleteProof {
  /**
   * Shorthand constructor that generates the Conclusion argument from the given values.
   */
  def apply(conc: ObjectRelation, crule: Rule, cargs: RuleArgs, proofPremises: Seq[Premise]): CompleteProof = {
    CompleteProof(Conclusion(conc, crule, cargs), proofPremises)
  }
}
