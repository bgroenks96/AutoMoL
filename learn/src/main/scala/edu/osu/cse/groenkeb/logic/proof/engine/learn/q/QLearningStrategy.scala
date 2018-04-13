package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import scala.collection.mutable.Map

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.engine.Success
import edu.osu.cse.groenkeb.logic.proof.engine.learn._
import scala.util.Random

final case class QLearningStrategy(model: QModel, policy: QPolicy, var alpha: Double = 0.1, val alphaDecay: Double = 1.0E-6, val alphaMin: Double = 1.0E-6)
    extends ProofStrategy {
  type Reward = Double
  private val RewardSuccess = 1
  private val RewardValidStep = 0.1
  private val RewardFailureWithContinue = -0.5
  private val RewardFailureTerminal = -1.0
  
  private val MaxCacheSize = 1000
  
  private val stateCache = Map[ProofContext, ProblemState]()
  private val pendingUpdates = Map[ProblemState, PendingUpdate]()
  
  def actions(implicit context: ProofContext): Seq[Action] = {
    val maybePrevState = context.parent match {
      case Some(parent) => Some(WorkingState(ProblemGraph(parent), None)) //Some(stateFor(parent, c => WorkingState(ProblemGraph(c), None)))
      case None => None
    }
    // Build problem state for current context
    val state = WorkingState(ProblemGraph(context), maybePrevState) //stateFor(context, c => WorkingState(ProblemGraph(c), maybePrevState))
    // Store state for future updates
    val availableActions = generateActions
    // Run update for pending parent state, if necessary
    maybePrevState match {
      case Some(prevState) if pendingUpdates.contains(prevState) =>
        val update = pendingUpdates(prevState)
        model.update(QUpdate(QArgs(prevState, update.action), state, update.reward, alpha), availableActions)
        updateAlpha
        pendingUpdates.remove(prevState)
      case _ => Unit
    }
    // Evaluate Q model for current state and select non-negative Q values
    val qvalues = model.evaluate(state, availableActions)
    policy.actionSequence(qvalues)
  }
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
  
  override def feedback(action: Action, result: ProofResult)(implicit context: ProofContext): ProofResult = result match {
    case Success(proof, context, Continue(_)) =>
      val prevState = WorkingState(ProblemGraph(context), None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val successState = SolvedState(proof, prevState)
      model.update(QUpdate(QArgs(prevState, action), successState, RewardSuccess, alpha), Nil)
      updateAlpha
      result
    case Failure(context, Continue(_)) =>
      val prevState = WorkingState(ProblemGraph(context), None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val failedState = FailedState(prevState)
      model.update(QUpdate(QArgs(prevState, action), failedState, RewardFailureWithContinue, alpha), Nil)
      updateAlpha
      result
    case Success(proof, context, Cut()) =>
      val prevState = WorkingState(ProblemGraph(context), None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val successState = SolvedState(proof, prevState)
      model.update(QUpdate(QArgs(prevState, action), successState, RewardSuccess, alpha), Nil)
      updateAlpha
      result
    case Failure(context, Cut()) =>
      val prevState = WorkingState(ProblemGraph(context), None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val failedState = FailedState(prevState)
      model.update(QUpdate(QArgs(prevState, action), failedState, RewardFailureTerminal, alpha), Nil)
      updateAlpha
      result
    case Pending(context,_,_) =>
      val prevState = WorkingState(ProblemGraph(context), None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      pendingUpdates(prevState) = PendingUpdate(action, RewardValidStep)
      result
  }
  
  private def generateActions(implicit context: ProofContext) =
    for {
      rule <- context.rules
      action <- context.available.filter { p => rule.major(p.sentence) } match {
        case avail if avail.isEmpty => Seq(Action(rule))
        case avail => avail.map { p => Action(rule, Some(p.sentence)) }.toSeq
      }
    } yield action
    
  private def updateAlpha {
      alpha = Math.max(alphaMin, alpha - alphaDecay)
    }
    
  private def stateFor(context: ProofContext, default: ProofContext => ProblemState) = stateCache.get(context) match {
      case Some(state) => state
      case None =>
        val state = default(context)
        store(context, state)
        state
    }
    
  private def store(context: ProofContext, state: ProblemState) {
    // if cache has exceeded max size, randomly drop a value
    if (stateCache.size > MaxCacheSize) stateCache.remove(stateCache.keys.toIndexedSeq(Random.nextInt(stateCache.size)))
    stateCache.put(context, state)
  }
    
  private case class PendingUpdate(action: Action, reward: Reward)
}
