package edu.osu.cse.groenkeb.logic.proof.engine.learn.qapprx

trait Feature[S, A] {
  def apply(state: S, action: A): Double
}
