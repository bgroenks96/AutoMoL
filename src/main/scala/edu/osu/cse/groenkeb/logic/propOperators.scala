package edu.osu.cse.groenkeb.logic

case class AndOp() extends BinaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => And(left, right) }

  def matches(op: Operator) = op match {
    case AndOp() => true
    case _ => false
  }

  override def toString() = "and"
}

case class OrOp() extends BinaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => Or(left, right) }

  def matches(op: Operator) = op match {
    case OrOp() => true
    case _ => false
  }

  override def toString() = "or"
}

case class ImpliesOp() extends BinaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => Implies(left, right) }

  def matches(op: Operator) = op match {
    case ImpliesOp() => true
    case _ => false
  }

  override def toString() = "=>"
}

case class NotOp() extends UnaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(operand) => Not(operand) }

  def matches(op: Operator) = op match {
    case NotOp() => true
    case _ => false
  }

  override def toString() = "~"
}
