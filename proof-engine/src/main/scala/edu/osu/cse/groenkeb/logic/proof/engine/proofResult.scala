package edu.osu.cse.groenkeb.logic.proof.engine

import scala.collection.immutable._
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.CompleteProof

sealed abstract class ProofResult extends Product with Serializable {
  def context: ProofContext
}
final case class Failure(context : ProofContext, hint: SearchHint = Cut()) extends ProofResult
final case class Success(proof: CompleteProof, context: ProofContext, hint: SearchHint = Cut()) extends ProofResult
final case class Pending(context: ProofContext,
                         steps: Seq[ProofStep],
                         aggregator: (ProofContext, Seq[Stream[ProofResult]]) => ProofResult) extends ProofResult
