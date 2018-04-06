package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import scala.collection.mutable.Map

import botkop.{ numsca => ns }
import botkop.numsca.Tensor
import org.nd4j.linalg.factory.Nd4j.PadMode

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.AutoLogician

final class QLogician(features: Seq[Feature[ProblemState, Action]], gamma: Double) extends AutoLogician[ProblemState, UpdateParams] {  
  // Sort by negative Q-value, thus ensuring descending order
  private implicit val OrderByQValue = Ordering[Double].on((qv: QValue) => -qv.value)
  
  private val biasTerm = Array(Array(0, 1))
  
  private val history = Map[QArgs, QValue]()
  
  private var weights = ns.ones(features.length + 1)
  
  def consult(state: ProblemState, availableActions: Seq[Action]): Seq[Action] = {
    // Compute Q values for current state and each available action
    val qvals = computeQValues(state, availableActions)
    // Sort Q values in descending order
    val sortedQVals = qvals.sorted
    // Return the corresponding sorted list of actions, adding this (state, action) pair to the history
    // map in the process (so we can look up the QValue later during updates).
    sortedQVals.collect { case v@QValue(args@QArgs(_, action), _, _) => history.put(args, v); action }
  }
  
  def update(params: UpdateParams, availableActions: Seq[Action]) = history.get(params.args) match {
    case Some(qval) =>
      // Compute Q values for new state and available actions
      val newQVals = computeQValues(params.newState, availableActions)
      // Assume optimal behavior for next action
      val maxQVal = newQVals.max
      // Calculate delta term: reward + gamma*(q_max - q)
      val delta = params.reward + gamma*(maxQVal.value - qval.value)
      // Update weights using alpha adjusted gradient
      weights += params.alpha * delta * qval.fvals
      // Drop (state, action) record from history
      history.remove(params.args)
    case _ => ???
  }
  
  /**
   * Computes Q-values at the given state for each action.
   */
  private def computeQValues(state: ProblemState, actions: Seq[Action]) = {
    actions.map {
      a =>
        val fvals = evaluateFeatures(state, a)
        val qargs = QArgs(state, a)
        QValue(qargs, qfunc(fvals), fvals)
    }
  }
  
  /**
   * Evaluates feature values for the given state and action.
   */
  private def evaluateFeatures(state: ProblemState, action: Action): Tensor = {
    val fvals = for {
      f <- features
    } yield f.eval(state, action)
    Tensor(fvals.toArray)
  }
  
  /**
   * Computes the raw Q-value for the given feature vector using the current weights.
   */
  private def qfunc(fvals: Tensor): Double = {
    val fvalsWithBias = ns.pad(fvals, biasTerm, PadMode.CONSTANT)
    fvalsWithBias(-1) := 1
    ns.dot(this.weights, fvalsWithBias.T).squeeze()
  }
}

final case class UpdateParams(newState: ProblemState, args: QArgs, reward: Double, alpha: Double)
