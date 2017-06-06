package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.types.Proof

import scala.collection.immutable._

abstract class ProofResult(val context: ProofContext)
case class Failure(cntxt: ProofContext, val hint: SearchHint = Cut()) extends ProofResult(cntxt)
case class Success(val proof: CompleteProof, cntxt: ProofContext, val hint: SearchHint = Cut()) extends ProofResult(cntxt)
case class Pending(cntxt: ProofContext,
                   steps: Seq[ProofStep],
                   aggregator: (ProofContext, Seq[Stream[ProofResult]]) => ProofResult) extends ProofResult(cntxt)
