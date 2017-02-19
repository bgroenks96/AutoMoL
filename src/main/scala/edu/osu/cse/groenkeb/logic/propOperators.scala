package edu.osu.cse.groenkeb.logic

case class And() extends BinaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => AndRelation(left, right) }

  def matches(op: Operator) = op match {
    case And() => true
    case _ => false
  }

  override def toString() = "and"
}

case class Or() extends BinaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => OrRelation(left, right) }

  def matches(op: Operator) = op match {
    case Or() => true
    case _ => false
  }

  override def toString() = "or"
}

case class Implies() extends BinaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => ImpliesRelation(left, right) }

  def matches(op: Operator) = op match {
    case Implies() => true
    case _ => false
  }

  override def toString() = "=>"
}

case class Not() extends UnaryOperator {
  def toRelation(s: Sentence*) = s match { case Seq(operand) => NotRelation(operand) }

  def matches(op: Operator) = op match {
    case Not() => true
    case _ => false
  }

  override def toString() = "~"
}