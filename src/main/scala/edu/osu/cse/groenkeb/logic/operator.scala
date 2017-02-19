package edu.osu.cse.groenkeb.logic

abstract class Operator {
  def toRelation(s: Sentence*): ObjectRelation
  def matches(op: Operator): Boolean
  override def toString(): String
}

abstract class BinaryOperator extends Operator
abstract class UnaryOperator extends Operator

case class Null() extends Operator {
  def toRelation(s: Sentence*) = s match { case Nil => NullObjectRelation() }
  
  def matches (op: Operator) = op match {
    case Null() => true
    case _ => false
  }
  
  override def toString() = ""
}
