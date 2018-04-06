package edu.osu.cse.groenkeb.logic.proof.engine.qlearn

trait Feature[S, A] {
  def eval[S, A](state: S, action: A): Double
}
