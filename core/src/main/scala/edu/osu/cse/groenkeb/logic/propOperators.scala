package edu.osu.cse.groenkeb.logic

case class And() extends BinaryConnective {
  def matches(op: Operator) = op match {
    case And() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.forall(functor)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "and"
}

case class Or() extends BinaryConnective {
  def matches(op: Operator) = op match {
    case Or() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.exists(functor)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "or"
}

case class Implies() extends BinaryConnective {
  def matches(op: Operator) = op match {
    case Implies() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => !functor(left) || functor(right)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "if"
}

case class Not() extends UnaryConnective {
  def matches(op: Operator) = op match {
    case Not() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(unary) => !functor(unary)
    case _ => throw new IllegalArgumentException(args.toString)
  }

  override def toString() = "not"
}