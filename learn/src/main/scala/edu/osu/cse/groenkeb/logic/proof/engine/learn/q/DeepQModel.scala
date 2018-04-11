package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import ml.dmlc.mxnet._
import ml.dmlc.mxnet.module.Module

import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy.Action
import edu.osu.cse.groenkeb.logic.proof.ProofContext

final class DeepQModel extends QModel {
  private val ctx = Context.defaultCtx
  
  def update(availableActions: Seq[Action])(implicit context: ProofContext): Seq[Action] = ???
}
