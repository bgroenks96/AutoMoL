package edu.osu.cse.groenkeb.logic

sealed abstract class Parity {
  def unary_~ = this match {
    case Positive => Negative
    case Negative => Positive
    case Mixed => Mixed
  }
  
  def toInt = this match {
    case Positive => 1
    case Negative => -1
    case Mixed => 0
  }
  
  def *(p: Parity) = this.toInt * p.toInt match {
    case 1 => Positive
    case -1 => Negative
    case 0 => Mixed
  }
  
  def +(p: Parity) = {
    if (p == this) p
    else Mixed
  }
}
case object Positive extends Parity
case object Negative extends Parity
case object Mixed extends Parity
