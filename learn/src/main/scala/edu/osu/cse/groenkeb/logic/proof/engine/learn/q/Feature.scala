package edu.osu.cse.groenkeb.logic.proof.engine.learn.q

import botkop.numsca.Tensor
import edu.osu.cse.groenkeb.logic.proof.Action
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

trait Feature {
  def apply(state: ProblemState, action: Action): Double
}