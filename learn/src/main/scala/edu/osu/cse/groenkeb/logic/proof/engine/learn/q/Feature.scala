package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import botkop.numsca.Tensor
import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

import scala.language.implicitConversions

trait Feature {
  def apply(state: ProblemState, action: Action): Double
}

object Feature {
  implicit def funcToFeature[Feature](func: (ProblemState, Action) => Double) = FeatureFuncWrapper(func)
  
  final case class FeatureFuncWrapper(func: (ProblemState, Action) => Double) extends Feature {
    def apply(state: ProblemState, action: Action) = func(state, action)
  }
}
