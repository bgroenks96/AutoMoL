package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

trait Feature[S, A] {
  def eval[S, A](state: S, action: A): Double
}
