package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic.proof.Proof
import scala.annotation.tailrec
import scala.collection.immutable._

import edu.osu.cse.groenkeb.logic.proof.ProofContext

case class ProofSearch(step: ProofStep) {
  def apply(): Stream[ProofResult] = {
    // continue constructs a new Stream with the head being the given result and the tail being the
    // Stream returned by a recursive search on the continuation step. Note that, per the nature of Streams,
    // the search will not be evaluated until the Stream itself is evaluated.
    def continue(result: ProofResult): Stream[ProofResult] = result match {
      case Success(p, context, Continue(step)) => Stream.cons(result, { ProofSearch(step)() })
      case Failure(context, Continue(step)) => Stream.cons(result, { ProofSearch(step)() })
      case result => Stream(result)
    }
    // descend recursively evaluates Pending results by running a lazy recursive search for all sub-steps and making
    // a tail recursive call with the result of the aggregator function.
    @tailrec
    def descend(result: ProofResult): Stream[ProofResult] = result match {
      case Pending(context, steps, aggregator) => descend(aggregator(context,
                                                                     steps.map { step => ProofSearch(step)() }))
      case result => expand(result)
    }
    // expand evaluates the given result and returns one of the following:
    // 1) a singleton Stream containing the results (for Cut cases)
    // 2) a pipelined Stream of continuation results, recursively including (1) and (3)
    // 3) a pipelined Stream of child-step results, recursively including (1) and (2)
    def expand(result: ProofResult): Stream[ProofResult] = result match {
      case Success(p, context, Cut()) => Stream(Success(p, context, Cut()))
      case Failure(context, Cut()) => Stream(Failure(context, Cut()))
      case Success(p, context, Continue(step)) => Stream(Success(p, context, Cut())) //continue(result)
      case Failure(context, Continue(step)) => continue(result)
      case result:Pending => descend(result)
    }
    
    // Evaluate the step function and return the expansion.
    expand(step())
  }
}

object ProofStep {
  def apply(stepFunc: => ProofResult)(implicit context: ProofContext) = new ProofStep(stepFunc)
}

class ProofStep private (stepFunc: => ProofResult)(implicit val context: ProofContext) {
  def apply(): ProofResult = stepFunc
  
  /**
   * Returns a new Step that composes the given Step with this one; 'next' will be
   * appended as a continuation of the Result of this Step.
   */
  def then(next: ProofStep): ProofStep = {
    def pipe(r: ProofResult): ProofResult = r match {
      case Success(p, c, Continue(s)) => Success(p, c, Continue(s.then(next)))
      case Failure(c, Continue(s)) => Failure(c, Continue(s.then(next)))
      case Success(p, c, Cut()) => Success(p, c, Continue(next))
      case Failure(c, Cut()) => Failure(c, Continue(next))
      // For Pending results, recursively feed the result of the aggregator function into 'pipe'
      case Pending(c, steps, agg) => Pending(c, steps, (aggCntxt, results) => pipe(agg(aggCntxt, results)))
    }
    
    ProofStep({ pipe(stepFunc) })
  }
}
