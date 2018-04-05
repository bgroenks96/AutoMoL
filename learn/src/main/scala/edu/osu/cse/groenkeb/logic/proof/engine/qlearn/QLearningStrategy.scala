package edu.osu.cse.groenkeb.logic.proof.engine.qlearn

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.engine.ProofResult

final case class QLearningStrategy() extends ProofStrategy {
  /**
   * Generates a sequence of possible Actions (inference rule + optional major premise)
   * in descending order of relevance/confidence that the solver should apply in searching
   * for a proof the given goal.
   */
  def actions(implicit context: ProofContext): Seq[ProofStrategy.Action] = ???
  
  /**
   * Returns an appropriate final ProofResult for the given ProofResult, according to this strategy.
   * This allows the ProofStrategy to make any necessary changes to the status of the proof result (like
   * Cut where the search engine would have defaulted to Continue, etc).
   */
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = ???
}
