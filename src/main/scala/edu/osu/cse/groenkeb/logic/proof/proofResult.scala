package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.types.Proof

import scala.collection.immutable._

sealed abstract class ProofResult extends Product with Serializable {
  def context: ProofContext
}
final case class Failure(context : ProofContext, hint: SearchHint = Cut()) extends ProofResult
final case class Success(proof: CompleteProof, context: ProofContext, hint: SearchHint = Cut()) extends ProofResult
final case class Pending(context: ProofContext,
                         steps: Seq[ProofStep],
                         aggregator: (ProofContext, Seq[Stream[ProofResult]]) => ProofResult) extends ProofResult
