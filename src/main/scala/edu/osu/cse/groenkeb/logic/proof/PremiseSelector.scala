package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.ObjectRelation

trait PremiseSelector {
  def select(conclusion: ObjectRelation)(implicit context: ProofContext): Seq[Premise]
}