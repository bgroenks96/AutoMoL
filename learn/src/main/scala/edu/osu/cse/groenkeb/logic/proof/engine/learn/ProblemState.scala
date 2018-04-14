package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.Proof

sealed abstract class ProblemState(parent: Option[ProblemState], context: ProofContext) {
  def goal = context.goal
  def assumptions = context.available
}
final case class WorkingState(graph: ProblemGraph, context: ProofContext, parent: Option[ProblemState]) extends ProblemState(parent, context)
final case class SolvedState(proof: Proof, context: ProofContext, parent: ProblemState) extends ProblemState(Some(parent), context)
final case class FailedState(context: ProofContext, parent: ProblemState) extends ProblemState(Some(parent), context)
