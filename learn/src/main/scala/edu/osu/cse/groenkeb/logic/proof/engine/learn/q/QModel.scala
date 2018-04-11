package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import edu.osu.cse.groenkeb.logic.proof.ProofContext

trait QModel {
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on((qv: QValue) => -qv.value)
  
  def update(availableActions: Seq[Action])(implicit context: ProofContext): Seq[Action]
}
