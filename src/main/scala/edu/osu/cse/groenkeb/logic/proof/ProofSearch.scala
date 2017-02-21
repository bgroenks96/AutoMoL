package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.types._

object ProofSearch {
  def findProof(sentence: Sentence)(implicit context: ProofContext): Seq[Proof] = {
    null
  }

  private def toRelations(premises: Seq[Premise]) = premises.map { p => p.sentence.toRelation }
}