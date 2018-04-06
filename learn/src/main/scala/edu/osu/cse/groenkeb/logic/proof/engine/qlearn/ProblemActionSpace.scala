package edu.osu.cse.groenkeb.logic.proof.engine.qlearn

import org.deeplearning4j.rl4j.space.ActionSpace

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action

import scala.util.Random

final case class ProblemActionSpace(actions: Seq[Action]) extends ActionSpace[Action] {
  
  private val random = Random
  
  def getSize = actions.size
    
  def randomAction: Action = actions(random.nextInt(getSize))
  
  def setSeed(seed: Int) = random.setSeed(seed)
  
  def encode(action: Action): Object = Int.box(action.hashCode())
  
  def noOp: Action = ???
}