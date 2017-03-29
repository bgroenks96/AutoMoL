package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext


//case class ProofSearch(val query: Sentence,
//                       val result: SearchResult,
//                       continue: Unit => Option[ProofSearch])(implicit context: ProofContext) {
//  def proof = result.proof
//  def next = result.hint match {
//    case Cut() => Option.empty[ProofSearch]
//    case Continue() => continue()
//  }
//}

abstract class ProofResult(val proof: Proof, val hint: SearchHint)
case class Failure(nullProof: NullProof, h: SearchHint = Continue()) extends ProofResult(nullProof, h)
case class Success(cp: CompleteProof, h: SearchHint = Continue()) extends ProofResult(cp, h)

abstract class SearchHint
case class Cut() extends SearchHint
case class Continue() extends SearchHint
