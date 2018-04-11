package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import edu.osu.cse.groenkeb.logic.proof.Action

final case class QArgs(state: ProblemState, action: Action)
final case class QValue(args: QArgs, value: Double)
final case class QUpdate(args: QArgs, newState: ProblemState, reward: Double, alpha: Double) {
  def oldState = args.state
  def action = args.action
}
