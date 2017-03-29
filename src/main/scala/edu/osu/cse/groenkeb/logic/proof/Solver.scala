package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext

trait Solver {
  def proof(implicit context: ProofContext): ProofResult
}
