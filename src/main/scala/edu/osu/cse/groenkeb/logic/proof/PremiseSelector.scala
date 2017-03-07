package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Sentence

trait PremiseSelector {
  def select(conclusion: Sentence)(implicit context: ProofContext): Seq[Premise]
}