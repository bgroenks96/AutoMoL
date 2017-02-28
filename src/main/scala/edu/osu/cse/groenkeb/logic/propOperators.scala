package edu.osu.cse.groenkeb.logic

case class AndOp() extends BinaryPredicate {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => And(left, right) }

  def matches[T](op: Operator[T]) = op match {
    case AndOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.forall(functor)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "and"
}

case class OrOp() extends BinaryPredicate {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => Or(left, right) }

  def matches[T](op: Operator[T]) = op match {
    case OrOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.exists(functor)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "or"
}

case class ImpliesOp() extends BinaryPredicate {
  def toRelation(s: Sentence*) = s match { case Seq(left, right) => Implies(left, right) }

  def matches[T](op: Operator[T]) = op match {
    case ImpliesOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => !functor(left) || functor(right)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "=>"
}

case class NotOp() extends UnaryPredicate {
  def toRelation(s: Sentence*) = s match { case Seq(operand) => Not(operand) }

  def matches[T](op: Operator[T]) = op match {
    case NotOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(unary) => !functor(unary)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "not"
}