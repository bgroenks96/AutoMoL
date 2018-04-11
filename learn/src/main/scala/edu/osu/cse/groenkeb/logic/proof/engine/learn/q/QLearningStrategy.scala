package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy
import edu.osu.cse.groenkeb.logic.proof.engine.ProofResult
import edu.osu.cse.groenkeb.logic.proof.engine.learn._

import scala.collection.mutable.Map

final case class QLearningStrategy() extends ProofStrategy {
  private val states = Map[ProofContext, ProblemState]()
  
  
  
  def actions(implicit context: ProofContext): Seq[Action] = {
    val parentState = context.parent match {
      case Some(parent) => states.get(parent) match {
        case Some(parentState) => Some(parentState)
        case None => ??? // shouldn't be possible to have untracked parent state
      }
      case None => None
    }
    // Build problem state for current context
    val state = WorkingState(ProblemGraph(context), parentState)
    // Store problem state for future updates
    states(context) = state
    
    ???
  }
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = {
    ???
  }
}
