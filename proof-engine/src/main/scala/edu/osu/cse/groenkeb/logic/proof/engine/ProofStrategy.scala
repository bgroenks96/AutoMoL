package edu.osu.cse.groenkeb.logic.proof.engine

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof._

trait ProofStrategy {
  /**
   * Generates a sequence of possible Actions (inference rule + optional major premise)
   * in descending order of relevance/confidence that the solver should apply in searching
   * for a proof the given goal.
   */
  def actions(implicit context: ProofContext): Seq[Action]
  
  /**
   * Returns an appropriate final ProofResult for the given ProofResult, according to this strategy.
   * This allows the ProofStrategy to make any necessary changes to the status of the proof result (like
   * Cut where the search engine would have defaulted to Continue, etc).
   */
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult
  
  /**
   * Provides explicit "feedback" to the strategy about the outcome of a particular action.
   * Returns the same result that was passed in for convenience to the caller (i.e. ProofSolver).
   */
  def feedback(action: Action, result: ProofResult, trace: Trace)(implicit context: ProofContext): ProofResult = result
}
