package edu.osu.cse.groenkeb.logic.proof.engine.qlearn

import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action

import org.deeplearning4j.rl4j.mdp.MDP
import org.deeplearning4j.rl4j.space.ObservationSpace
import org.deeplearning4j.gym.StepReply

final class Environment extends MDP[ProblemState, Action, ProblemActionSpace] {
  
  def getObservationSpace: ObservationSpace[ProblemState] = ???
  
  def getActionSpace: ProblemActionSpace = ???
  
  def reset: ProblemState = ???
  
  def close = ???
  
  def step(action: Action): StepReply[ProblemState] = ???
  
  def isDone: Boolean = false
  
  def newInstance: Environment = ???
}