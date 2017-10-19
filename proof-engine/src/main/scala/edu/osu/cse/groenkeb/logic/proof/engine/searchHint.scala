package edu.osu.cse.groenkeb.logic.proof.engine

sealed abstract class SearchHint {
  def ->(other: SearchHint): SearchHint
}
case class Cut() extends SearchHint {
  def ->(other: SearchHint) = other match {
    case h:Continue => h
    case h:Cut => this
  }
}
case class Continue(step: ProofStep) extends SearchHint {
  def ->(other: SearchHint) = other match {
    case Continue(altstep) => Continue(step.then(altstep))
    case Cut() => this
  }
}
