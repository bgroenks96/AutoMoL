package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.engine.learn._

import scala.collection.mutable.Map
import edu.osu.cse.groenkeb.logic.proof.engine.Success

final case class QLearningStrategy(model: QModel, policy: QPolicy) extends ProofStrategy {
  type Reward = Double
  private val RewardSuccess = 1
  private val RewardFailureWithContinue = -0.5
  private val RewardFailureTerminal = -1.0
  private val Alpha = 0.1f
  
  private val states = Map[ProofContext, ProblemState]()
  private val pendingRewards = Map[ProofContext, Reward]()
  
  def actions(implicit context: ProofContext): Seq[Action] = {
    val maybeParentState = context.parent match {
      case Some(parent) => states.get(parent) match {
        case Some(parentState) => Some(parentState)
        case None => ??? // shouldn't be possible to have untracked parent state
      }
      case None => None
    }
    // Build problem state for current context
    val state = WorkingState(ProblemGraph(context), maybeParentState)
    // Store state for future updates
    states(context) = state
    val availableActions = generateActions
    // Run update for pending parent state
    maybeParentState match {
      case Some(parentState) if pendingRewards.contains(context) =>
        assert(context.action.isDefined)
        val reward = pendingRewards(context)
        model.update(QUpdate(QArgs(parentState, context.action.get), state, reward, Alpha), availableActions)
        pendingRewards.remove(context)
      case _ => Unit
    }
    // Evaluate Q model for current state
    val qvalues = model.evaluate(state, availableActions)
    policy.actionSequence(qvalues)
  }
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = {
    result match {
      case Success(proof, context, Cut()) => context.parent match {
        case Some(parent) =>
          assert(states.contains(parent))
          assert(context.action.isDefined)
          val parentState = states(parent)
          val successState = SolvedState(proof, parentState)
          val args = QArgs(parentState, context.action.get)
          updateTerminal(QUpdate(args, successState, RewardSuccess, Alpha))
          states.remove(parent)
        case None => Unit
      }
      case Failure(context, Cut()) => context.parent match {
        case Some(parent) =>
          assert(states.contains(parent))
          assert(context.action.isDefined)
          val parentState = states(parent)
          val failureState = FailedState(parentState)
          val args = QArgs(parentState, context.action.get)
          updateTerminal(QUpdate(args, failureState, RewardFailureTerminal, Alpha))
          states.remove(parent)
        case None => Unit
      }
      case Success(proof, context, Continue(_)) => context.parent match {
        case Some(parent) => updateLater(RewardSuccess, parent)
        case None => Unit
      }
      case Failure(context, Continue(_)) => context.parent match {
        case Some(parent) => updateLater(RewardFailureWithContinue, parent)
        case None => Unit
      }
      case _ => Unit
    }
    result
  }
  
  private def updateTerminal(params: QUpdate) {
    model.update(params, Nil)
  }
  
  private def updateLater(reward: Reward, parent: ProofContext) {
    assert(!pendingRewards.contains(parent))
    pendingRewards(parent) = reward
  }
  
  private def generateActions(implicit context: ProofContext) =
    for {
      rule <- context.rules
      action <- context.available.filter { p => rule.major(p.sentence) } match {
        case avail if avail.isEmpty => Seq(Action(rule))
        case avail => avail.map { p => Action(rule, Some(p.sentence)) }.toSeq
      }
    } yield action
}
