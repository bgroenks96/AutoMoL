package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action

trait AutoLogician[TState, TParams] {
  
  def consult(state: TState, availableActions: Seq[Action]): Seq[Action]
  
  def update(params: TParams, availableActions: Seq[Action])
}
