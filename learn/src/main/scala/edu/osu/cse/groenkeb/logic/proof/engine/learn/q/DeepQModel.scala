package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState
import ml.dmlc.mxnet._

final class DeepQModel extends QModel {
  private implicit val ctx = Context.defaultCtx
  
  private var mode: QModel.Mode = QModel.TrainMode
  
  def evaluate(state: ProblemState, availableActions: Seq[Action]): Seq[QValue] = ???
  
  def update(params: QUpdate, availableActions: Seq[Action]): Unit = ???
  
  def setMode(mode: QModel.Mode) {
    this.mode = mode
  }
}
