package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

trait QModel {
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on((qv: QValue) => -qv.value)
  
  def computeQValues(state: ProblemState, actions: Seq[Action]): Seq[QValue]
  def update(params: QUpdate, availableActions: Seq[Action])
}
