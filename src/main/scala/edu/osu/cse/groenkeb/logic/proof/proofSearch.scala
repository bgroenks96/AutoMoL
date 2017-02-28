package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext

case class ProofSearch(val query: Sentence, val result: SearchResult)(implicit context: ProofContext) {
  def proof = result.proof
}

abstract class SearchResult(val proof: Proof)
case class Failure(nullProof: NullProof) extends SearchResult(nullProof)
case class Success(cp: CompleteProof, val continuation: Unit => ProofSearch) extends SearchResult(cp)