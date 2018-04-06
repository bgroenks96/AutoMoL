package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import botkop.numsca.Tensor

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action

final case class QArgs(state: ProblemState, action: Action)
final case class QValue(args: QArgs, value: Double, fvals: Tensor)
