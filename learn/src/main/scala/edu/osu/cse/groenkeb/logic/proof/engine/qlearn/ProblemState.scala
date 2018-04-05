package edu.osu.cse.groenkeb.logic.proof.engine.qlearn

import edu.osu.cse.groenkeb.logic.proof.engine.repr.SentenceGraph
import edu.osu.cse.groenkeb.logic.proof.ProofContext

object ProblemState {
  def apply(context: ProofContext, parent: Option[ProblemState] = None): ProblemState =
    ProblemState(SentenceGraph(context.goal), context.available.map(a => SentenceGraph(a.sentence)), parent)
}

final case class ProblemState(goal: SentenceGraph, available: Set[SentenceGraph], parent: Option[ProblemState])
