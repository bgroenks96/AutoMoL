package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

trait QModel {
  def computeQValues(state: ProblemState, actions: Seq[Action]): Seq[QValue]
  def update(params: QUpdate, availableActions: Seq[Action])
}
