package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

final case class QArgs(state: ProblemState, action: Action)
final case class QValue(args: QArgs, value: Double)
final case class QUpdate(newState: ProblemState, reward: Double, alpha: Double)
