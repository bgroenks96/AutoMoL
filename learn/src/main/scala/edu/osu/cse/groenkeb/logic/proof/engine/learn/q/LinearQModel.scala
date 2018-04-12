package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import org.nd4j.linalg.factory.Nd4j.PadMode
import botkop.{numsca => ns}
import botkop.numsca.Tensor

import scala.collection.mutable.Map
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemGraph
import edu.osu.cse.groenkeb.logic.proof.engine.learn.WorkingState
import edu.osu.cse.groenkeb.logic.proof.engine.learn.SolvedState
import edu.osu.cse.groenkeb.logic.proof.engine.learn.FailedState

final class LinearQModel(features: Seq[Feature], gamma: Double) extends QModel {
  // Represents Q value + feature values
  type QResult = (QValue, Tensor)
  
  // Sort by negative Q-value, thus ensuring descending order
  implicit val orderByHighestQValue = Ordering[Double].on[QResult]((qv: QResult) => qv._1.value)

  def update(availableActions: Seq[Action])(implicit context: ProofContext): Seq[Action] = ???
  
  private val biasTermPadShape = Array(Array(0, 0), Array(0, 1))
  
  private var weights = ns.ones(features.length + 1)
  
  def evaluate(state: ProblemState, availableActions: Seq[Action]): Seq[QValue] = {
    // Compute Q values for current state and each available action
    val qvals = computeQValues(state, availableActions)
    // Sort Q values in descending order
    qvals.sorted.collect { case (qv, _) => qv }
  }
  
  def update(params: QUpdate, availableActions: Seq[Action]) = availableActions match {
    case Nil => None
    case _ =>
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
    val fvals = features.map(f => f(state, action))
    Tensor(fvals.toArray)
  }
  
  /**
   * Computes the raw Q-value for the given feature vector using the current weights.
   */
  private def qfunc(fvals: Tensor): Double = {
    val fvalsWithBias = ns.pad(fvals, biasTermPadShape, PadMode.CONSTANT)
    fvalsWithBias(fvalsWithBias.shape(1) - 1) := 1
    ns.dot(this.weights, fvalsWithBias.T).squeeze()
  }
}
