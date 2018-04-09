package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.ProofResult
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

final case class QLearningStrategy() extends ProofStrategy {
  def actions(implicit context: ProofContext): Seq[ProofStrategy.Action] = ???
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = ???
}
