package edu.osu.cse.groenkeb.logic

abstract class Operator
{
  def relate(s1: Sentence)(s2: Sentence): Relation
  def matches(op: Operator): Boolean
  override def toString(): String
}

abstract class BinaryOperator extends Operator
abstract class UnaryOperator extends Operator

case class And() extends BinaryOperator
{
  def relate(s1: Sentence)(s2: Sentence) = AndRelation(s1, s2)
  
  def matches(op: Operator) = op match
  {
    case And() => true
    case _ => false
  }
  
  override def toString() = "and"
}

case class Or() extends BinaryOperator
{
  def relate(s1: Sentence)(s2: Sentence) = OrRelation(s1, s2)
  
  def matches(op: Operator) = op match
  {
    case And() => true
    case _ => false
  }
  
  override def toString() = "or"
}

case class Implies() extends BinaryOperator
{
  def relate(s1: Sentence)(s2: Sentence) = ImpliesRelation(s1, s2)
  
  def matches(op: Operator) = op match
  {
    case Implies() => true
    case _ => false
  }
  
  override def toString() = "=>"
}

case class Not() extends UnaryOperator
{
  def relate(s1: Sentence)(s2: Sentence) = NotRelation(s1)
  
  def matches(op: Operator) = op match
  {
    case Not() => true
    case _ => false
  }
  
  override def toString() = "~"
}