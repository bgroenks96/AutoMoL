package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

import org.deeplearning4j.rl4j.space.ActionSpace
import org.deeplearning4j.rl4j.space.DiscreteSpace

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action

final case class ProblemActionSpace(actions: Seq[Action]) extends ActionSpace[Action] {
  
  private val discreteSpace = new DiscreteSpace(getSize)
  
  def getSize = actions.size
    
  def randomAction: Action = actions(discreteSpace.randomAction())
  
  def setSeed(seed: Int) = discreteSpace.setSeed(seed)
  
  def encode(action: Action): Object = discreteSpace.encode(action.hashCode())
  
  def noOp: Action = ???
}