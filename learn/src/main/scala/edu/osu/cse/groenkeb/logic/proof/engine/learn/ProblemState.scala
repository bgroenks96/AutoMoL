package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.Proof

sealed abstract class ProblemState(parent: Option[ProblemState])
final case class WorkingState(graph: ProblemGraph, parent: Option[ProblemState]) extends ProblemState(parent)
final case class SolvedState(proof: Proof, parent: ProblemState) extends ProblemState(Some(parent))
final case class FailedState(parent: ProblemState) extends ProblemState(Some(parent))
