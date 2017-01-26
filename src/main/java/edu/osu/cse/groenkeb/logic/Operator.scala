package edu.osu.cse.groenkeb.logic

import edu.osu.cse.groenkeb.logic.relation.Relation
import edu.osu.cse.groenkeb.logic.relation.Relations

abstract class Operator
{
  def asRelation(): Relation
  def matches(op: Operator): Boolean
  override def toString(): String
}

abstract class BinaryOperator extends Operator
abstract class UnaryOperator extends Operator

case class And() extends BinaryOperator
{
  def asRelation() = Relations.and()
  
  def matches(op: Operator) = op match
  {
    case And() => true
    case _ => false
  }
  
  override def toString() = "&"
}

case class Not() extends UnaryOperator
{
  def asRelation() = Relations.not()
  
  def matches(op: Operator) = op match
  {
    case Not() => true
    case _ => false
  }
  
  override def toString() = "~"
}