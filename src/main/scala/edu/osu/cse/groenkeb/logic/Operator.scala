package edu.osu.cse.groenkeb.logic

abstract class Operator {
  def relate(s: Sentence*): Relation
  def matches(op: Operator): Boolean
  override def toString(): String
}

abstract class BinaryOperator extends Operator
abstract class UnaryOperator extends Operator

case class And() extends BinaryOperator {
  def relate(s: Sentence*) = s match { case _ :: _ :: Nil => AndRelation(s(0), s(1)) }

  def matches(op: Operator) = op match {
    case And() => true
    case _ => false
  }

  override def toString() = "and"
}

case class Or() extends BinaryOperator {
  def relate(s: Sentence*) = s match { case _ :: _ :: Nil => OrRelation(s(0), s(1)) }

  def matches(op: Operator) = op match {
    case And() => true
    case _ => false
  }

  override def toString() = "or"
}

case class Implies() extends BinaryOperator {
  def relate(s: Sentence*) = s match { case _ :: _ :: Nil => ImpliesRelation(s(0), s(1)) }

  def matches(op: Operator) = op match {
    case Implies() => true
    case _ => false
  }

  override def toString() = "=>"
}

case class Not() extends UnaryOperator {
  def relate(s: Sentence*) = s match { case _ :: Nil => NotRelation(s(0)) }

  def matches(op: Operator) = op match {
    case Not() => true
    case _ => false
  }

  override def toString() = "~"
}

case class Null() extends Operator {
  def relate(s: Sentence*) = s match { case Nil => NullRelation() }
  
  def matches (op: Operator) = op match {
    case Null() => true
    case _ => false
  }
  
  override def toString() = ""
}
