package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import scala.collection.mutable.Map

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.AutoLogician
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

final class QLogician(model: QModel, gamma: Double) extends AutoLogician[ProblemState, QUpdate] {  
  // Sort by negative Q-value, thus ensuring descending order
  private implicit val orderByQValue = Ordering[Double].on((qv: QValue) => -qv.value)
  
  private val biasTermPadShape = Array(Array(0, 1))
  
  private val history = Map[QArgs, QValue]()
  
  def consult(state: ProblemState, availableActions: Seq[Action]): Seq[Action] = {
    // Compute Q values for current state and each available action
    val qvals = computeQValues(state, availableActions)
    // Sort Q values in descending order
    val sortedQVals = qvals.sorted
    // Return the corresponding sorted list of actions, adding this (state, action) pair to the history
    // map in the process (so we can look up the QValue later during updates).
    sortedQVals.collect { case v@QValue(args@QArgs(_, action), _) => history.put(args, v); action }
  }
  
  def update(params: QUpdate, availableActions: Seq[Action]) = history.get(params.args) match {
    case Some(qval) =>
      // Drop (state, action) record from history
      history.remove(params.args)
    case _ => ???
  }
  
  def computeQValues(state: ProblemState, actions: Seq[Action]): Seq[QValue] = ???
}
