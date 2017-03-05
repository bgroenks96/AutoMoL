package edu.osu.cse.groenkeb.logic

case class AndOp() extends BinaryConnective {
  def matches(op: Operator) = op match {
    case AndOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.forall(functor)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "and"
}

case class OrOp() extends BinaryConnective {
  def matches(op: Operator) = op match {
    case OrOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.exists(functor)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "or"
}

case class ImpliesOp() extends BinaryConnective {
  def matches(op: Operator) = op match {
    case ImpliesOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => !functor(left) || functor(right)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "=>"
}

case class NotOp() extends UnaryConnective {
  def matches(op: Operator) = op match {
    case NotOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(unary) => !functor(unary)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "not"
}