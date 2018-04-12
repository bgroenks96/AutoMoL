package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import ml.dmlc.mxnet._
import ml.dmlc.mxnet.module.Module

import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import edu.osu.cse.groenkeb.logic.proof._

final class DeepQModel extends QModel {
  private implicit val ctx = Context.defaultCtx
  
  def evaluate(state: ProblemState, availableActions: Seq[Action]): Seq[QValue] = ???
  
  def update(params: QUpdate, availableActions: Seq[Action]): Option[QValue] = ???
}
