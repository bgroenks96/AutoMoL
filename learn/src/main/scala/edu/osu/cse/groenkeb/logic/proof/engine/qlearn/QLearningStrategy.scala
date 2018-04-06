package edu.osu.cse.groenkeb.logic.proof.engine.qlearn

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.ProofResult

final case class QLearningStrategy(features: Seq[Feature[ProblemState, Action]]) extends ProofStrategy {
  def actions(implicit context: ProofContext): Seq[ProofStrategy.Action] = ???
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = ???
}
