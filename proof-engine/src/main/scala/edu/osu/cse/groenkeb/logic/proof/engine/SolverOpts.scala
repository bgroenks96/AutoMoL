package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic.proof.ProofContext

sealed abstract class SolverOpts
final case class Trace(state: ProofTrace = new ProofTrace(_ => Unit)) extends SolverOpts {
  def stepCount = state.stepCount
}
                 
final class ProofTrace(private val stepCallback: ProofContext => Unit) {
  private var steps = 0
  
  private[engine] def trace(implicit context: ProofContext) = {
    steps += 1
    stepCallback(context)
  }
  
  private[engine] def reset = steps = 0
  
  def stepCount = steps
}
