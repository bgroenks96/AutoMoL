package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import org.nd4j.linalg.factory.Nd4j.PadMode

import botkop.{ numsca => ns }
import botkop.numsca.Tensor
import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

final class LinearQModel(features: Seq[Feature], gamma: Double) extends QModel {
  // Represents Q value + feature values
  type QResult = (QValue, Tensor)
  
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on[QResult]((qv: QResult) => qv._1.value)

  def update(availableActions: Seq[Action])(implicit context: ProofContext): Seq[Action] = ???
  
  var weights = ns.ones(features.length + 1)
  
  private var mode: QModel.Mode = QModel.TrainMode
  
  def evaluate(state: ProblemState, availableActions: Seq[Action]): Seq[QValue] = {
    // Compute Q values for current state and each available action
    val qvals = computeQValues(state, availableActions)
    // Sort Q values in descending order
    qvals.sorted.collect { case (qv, _) => qv }
  }
  
  def update(params: QUpdate, availableActions: Seq[Action]) = availableActions match {
    case Nil => None
    case _ if this.mode == QModel.TrainMode =>
      // Re-compute expected Q value for the previous state and selected action
      // This is necessary because of the recursive nature of proof search; the weights
      // may have been updated one or more times since the original Q value was computed
      val (qval, fvals) = computeQValue(params.oldState)(params.action)
      // Compute Q values for new state and available actions
      val newQVals = computeQValues(params.newState, availableActions)
      // Assume optimal behavior for next action
      val (maxQVal, _) = newQVals.max
      // Calculate delta term: r + gamma*q_max - q
      val delta = params.reward + gamma*maxQVal.value - qval.value
      // Update weights using alpha adjusted gradient
      weights += params.alpha * delta * fvals
      Some(maxQVal)
    case _ => None
  }
  
  def setMode(mode: QModel.Mode) {
    this.mode = mode
  }
  
  /**
   * Computes Q-values at the given state for each action.
   */
  private def computeQValues(state: ProblemState, actions: Seq[Action]) = actions.map(computeQValue(state))
  
  private def computeQValue(state: ProblemState)(action: Action) = {
    val fvals = evaluateFeatures(state, action)
    val qargs = QArgs(state, action)
    (QValue(qargs, qfunc(fvals)), fvals)
  }
  
  /**
   * Evaluates feature values for the given state and action.
   */
  private def evaluateFeatures(state: ProblemState, action: Action): Tensor = {
    Tensor(features.map(f => f(state, action)).toArray :+ 1.0)
  }
  
  /**
   * Computes the raw Q-value for the given feature vector using the current weights.
   */
  private def qfunc(fvals: Tensor): Double = {
    ns.dot(this.weights, fvals.T).squeeze()
  }
}