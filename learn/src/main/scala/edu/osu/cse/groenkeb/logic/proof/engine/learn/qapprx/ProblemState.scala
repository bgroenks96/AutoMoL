package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import edu.osu.cse.groenkeb.logic.proof.engine.learn.SentenceGraph
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.Proof

object ProblemState {
  def createFrom(context: ProofContext, parent: Option[ProblemState] = None): WorkingState =
    WorkingState(SentenceGraph(context.goal) ++
                 context.available.map(p => SentenceGraph(p.sentence)).reduce((a, b) => a ++ b),
                 parent)
}

sealed abstract class ProblemState(parent: Option[ProblemState])
final case class WorkingState(graph: SentenceGraph, parent: Option[ProblemState]) extends ProblemState(parent)
final case class SolvedState(proof: Proof, parent: Option[ProblemState]) extends ProblemState(parent)
final case class FailedState(parent: Option[ProblemState]) extends ProblemState(parent)
