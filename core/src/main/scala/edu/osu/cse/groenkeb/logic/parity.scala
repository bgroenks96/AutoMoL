package edu.osu.cse.groenkeb.logic

sealed abstract class Parity {
  def unary_~ = this match {
    case Positive => Negative
    case Negative => Positive
  }
}
case object Positive extends Parity
case object Negative extends Parity
