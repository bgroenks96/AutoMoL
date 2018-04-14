package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import edu.osu.cse.groenkeb.logic.proof.Action

trait QModel {
  /**
   * Returns a set of Q values for each available action in descending order,
   * i.e. highest expected reward first.
   */
  def evaluate(state: ProblemState, availableActions: Seq[Action]): Seq[QValue]
  
  /**
   * Updates the model using the state/action/reward values given in 'params'.
   */
  def update(params: QUpdate, availableActions: Seq[Action]): Unit
  
  def setMode(mode: QModel.Mode)
}

object QModel {
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on((qv: QValue) => -qv.value)
  
  sealed abstract class Mode
  case object TrainMode extends Mode
  case object TestMode extends Mode
}
