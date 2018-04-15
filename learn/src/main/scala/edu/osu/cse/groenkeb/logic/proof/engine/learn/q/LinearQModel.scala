package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import org.nd4j.linalg.factory.Nd4j.PadMode

import botkop.{ numsca => ns }
import botkop.numsca.Tensor
import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

final class LinearQModel(features: Seq[Feature], gamma: Double) extends QModel {
  // Represents Q value + feature values
  type QResult = (QValue, Array[Double])
  
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on[QResult]((qv: QResult) => qv._1.value)

  def update(availableActions: Seq[Action])(implicit context: ProofContext): Seq[Action] = ???
  
  var weights = ns.zeros(features.length + 1).data
  
  private var mode: QModel.Mode = QModel.TrainMode
  
  def evaluate(state: ProblemState, availableActions: Seq[Action]): Seq[QValue] = {
    // Compute Q values for current state and each available action
    val qvals = computeQValues(state, availableActions)
    // Sort Q values in descending order
    qvals.sorted.collect { case (qv, _) => qv }
  }
  
  def update(params: QUpdate, availableActions: Seq[Action]) = {
    if (this.mode == QModel.TrainMode) {
      // Re-compute expected Q value for the previous state and selected action
      // This is necessary because of the recursive nature of proof search; the weights
      // may have been updated one or more times since the original Q value was computed
      val (qval, fvals) = computeQValue(params.oldState)(params.action)
      // Compute Q values for new state and available actions
      val newQVals = computeQValues(params.newState, availableActions)
      // Assume optimal behavior for next action
      val maxQVal = if (newQVals.isEmpty) 0.0 else newQVals.max._1.value
      // Calculate delta term: r + gamma*q_max - q
      val delta = params.reward + gamma*maxQVal - qval.value
      // Update weights using alpha adjusted gradient
      for (i <- 0 until weights.length) {
        weights(i) += params.alpha*delta*fvals(i)
      }
      //weights += params.alpha * delta * fvals
    }
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
  private def evaluateFeatures(state: ProblemState, action: Action): Array[Double] = {
    (features.map(f => f(state, action)) :+ 1.0).toArray
  }
  
  /**
   * Computes the raw Q-value for the given feature vector using the current weights.
   */
  private def qfunc(fvals: Array[Double]): Double = {
    this.weights.zip(fvals).map { case (w, f) => w*f }.sum
    //ns.dot(this.weights, fvals.T).squeeze()
  }
}
