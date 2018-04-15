package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import scala.collection.mutable.Map

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.engine.Success
import edu.osu.cse.groenkeb.logic.proof.engine.learn._
import scala.util.Random
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule

final case class QLearningStrategy(model: QModel,
                                   policy: QPolicy,
                                   var alpha: Double = 0.1,
                                   val alphaDecay: Double = 1.0E-6,
                                   val alphaMin: Double = 1.0E-6,
                                   updateCallback: (ProblemState, Double) => Unit = (_,_) => Unit)
    extends ProofStrategy {
  type Reward = Double
  private val RewardValidStep = 0.0
  private val RewardFailureWithContinue = 0.0
  private val RewardFailureTerminal = 0.0
  
  private val stateCache = Map[ProofContext, ProblemState]()
  private val pendingUpdates = Map[ProblemState, PendingUpdate]()
  
  def actions(implicit context: ProofContext) = context.goal match {
    case s@AtomicSentence(_) if context.available.forall(p => !p.sentence.accessible(s)) => Nil
    case _ =>
      val maybePrevState = context.parent match {
        case Some(parent) => Some(WorkingState(ProblemGraph(parent), parent, None)) //Some(stateFor(parent, c => WorkingState(ProblemGraph(c), None)))
        case None => None
      }
      // Build problem state for current context
      val state = WorkingState(ProblemGraph(context), context, maybePrevState) //stateFor(context, c => WorkingState(ProblemGraph(c), maybePrevState))
      // Store state for future updates
      val availableActions = generateActions
      // Run update for pending parent state, if necessary
      maybePrevState match {
        case Some(prevState) if pendingUpdates.contains(prevState) =>
          val update = pendingUpdates(prevState)
          model.update(QUpdate(QArgs(prevState, update.action), state, update.reward, alpha), availableActions)
          updateAlpha
          updateCallback(state, update.reward)
          pendingUpdates.remove(prevState)
        case _ => Unit
      }
      // Evaluate Q model for current state and select non-negative Q values
      val qvalues = model.evaluate(state, availableActions)
      //val filteredQValues = qvalues.filter(qv => qv.value >= 0)
      policy.actionSequence(qvalues)
  }
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
  
  override def feedback(action: Action, result: ProofResult, trace: Trace)(implicit context: ProofContext): ProofResult = result match {
    case Success(proof, context, Continue(_)) =>
      val prevState = WorkingState(ProblemGraph(context), context, None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val successState = SolvedState(proof, context, prevState)
      val reward = calculateReward(proof, trace.stepCount)
      model.update(QUpdate(QArgs(prevState, action), successState, reward, alpha), Nil)
      updateAlpha
      updateCallback(successState, reward)
      result
    case Failure(context, Continue(_)) =>
      val prevState = WorkingState(ProblemGraph(context), context, None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val failedState = FailedState(context, prevState)
      model.update(QUpdate(QArgs(prevState, action), failedState, RewardFailureWithContinue, alpha), Nil)
      updateAlpha
      updateCallback(failedState, RewardFailureWithContinue)
      result
    case Success(proof, context, Cut()) =>
      val prevState = WorkingState(ProblemGraph(context), context, None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val successState = SolvedState(proof, context, prevState)
      val reward = calculateReward(proof, trace.stepCount)
      model.update(QUpdate(QArgs(prevState, action), successState, reward, alpha), Nil)
      updateAlpha
      updateCallback(successState, reward)
      result
    case Failure(context, Cut()) =>
      val prevState = WorkingState(ProblemGraph(context), context, None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      val failedState = FailedState(context, prevState)
      model.update(QUpdate(QArgs(prevState, action), failedState, RewardFailureTerminal, alpha), Nil)
      updateAlpha
      updateCallback(failedState, RewardFailureTerminal)
      result
    case Pending(context,_,_) =>
      val prevState = WorkingState(ProblemGraph(context), context, None) //stateFor(context, c => WorkingState(ProblemGraph(c), None))
      pendingUpdates(prevState) = PendingUpdate(action, RewardValidStep)
      result
  }
  
  private def generateActions(implicit context: ProofContext) =
    for {
      rule <- filterRules
      action <- context.available.filter { p => rule.major(p.sentence) } match {
        case avail if avail.isEmpty => Seq(Action(rule))
        case avail => avail.map { p => Action(rule, Some(p.sentence)) }.toSeq
      }
    } yield action
    
  private def calculateReward(proof: Proof, totalStepCount: Int) = -Math.log(ProofUtils.countSteps(proof) / totalStepCount.toDouble)
    
  private def filterRules(implicit context: ProofContext): Seq[Rule] = context.rules.collect {
    case r@AndElimination => r
    case r@OrElimination => r
    case r@NegationElimination /*if context.goal is Absurdity*/ => r
    case r@AndIntroduction if context.goal has And => r
    case r@IfIntroduction if context.goal has If => r
    case r@NegationIntroduction if context.goal has Not => r
    case r@IfElimination if (context.goal.isAtomic) or (context.goal has Or) => r
    case r@OrIntroduction if context.goal has Or => r
  }

  private def updateAlpha {
      alpha = Math.max(alphaMin, alpha - alphaDecay)
    }
    
//  private def stateFor(context: ProofContext, default: ProofContext => ProblemState) = stateCache.get(context) match {
//      case Some(state) => state
//      case None =>
//        val state = default(context)
//        store(context, state)
//        state
//    }
//    
//  private def store(context: ProofContext, state: ProblemState) {
//    // if cache has exceeded max size, randomly drop a value
//    if (stateCache.size > MaxCacheSize) stateCache.remove(stateCache.keys.toIndexedSeq(Random.nextInt(stateCache.size)))
//    stateCache.put(context, state)
//  }
    
  private case class PendingUpdate(action: Action, reward: Reward)
}
