package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import edu.osu.cse.groenkeb.logic.proof.Action

trait QModel {
  def consult(state: ProblemState, availableActions: Seq[Action]): Seq[QValue]
  
  def update(params: QUpdate, availableActions: Seq[Action]): QValue
}

object QModel {
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on((qv: QValue) => -qv.value)
}
