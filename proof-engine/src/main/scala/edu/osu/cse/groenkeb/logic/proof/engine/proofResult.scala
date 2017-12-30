package edu.osu.cse.groenkeb.logic.proof.engine

import scala.collection.immutable._
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.Proof

sealed abstract class ProofResult extends Product with Serializable {
  def context: ProofContext
}
final case class Failure(context : ProofContext, hint: SearchHint = Cut()) extends ProofResult
final case class Success(proof: Proof, context: ProofContext, hint: SearchHint = Cut()) extends ProofResult
final case class Pending(context: ProofContext,
                         steps: Seq[ProofStep],
                         aggregator: (ProofContext, Seq[Stream[ProofResult]]) => ProofResult) extends ProofResult {
  def andThen(step: ProofStep) = 
    Pending(context, steps, (c, res) => ProofStep({
      println("andThen: " + step.context)
      aggregator(c, res)
    })(c).then(step)())
}
