package edu.osu.cse.groenkeb.logic

case object And extends BinaryConnective {
  def matches(op: Operator) = op match {
    case And => true
    case _ => false
  }
  
  def evaluate(func: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.forall(func)
    case _ => throw new IllegalArgumentException(args.toString)
  }
  
  def apply(left: Sentence, right: Sentence) = BinarySentence(left, right, And)
  
  def unapply(sentence: Sentence) = sentence match {
    case BinarySentence(left, right, And) => Some((left, right))
    case _ => None
  }

  override def toString() = "and"
}

case object Or extends BinaryConnective {
  def matches(op: Operator) = op match {
    case Or => true
    case _ => false
  }
  
  def evaluate(func: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => args.exists(func)
    case _ => throw new IllegalArgumentException(args.toString)
  }
  
  def apply(left: Sentence, right: Sentence) = BinarySentence(left, right, Or)
  
  def unapply(sentence: Sentence) = sentence match {
    case BinarySentence(left, right, Or) => Some((left, right))
    case _ => None
  }

  override def toString() = "or"
}

case object Implies extends BinaryConnective {
  def matches(op: Operator) = op match {
    case Implies => true
    case _ => false
  }
  
  def evaluate(func: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(left, right) => !func(left) || func(right)
    case _ => throw new IllegalArgumentException(args.toString)
  }
  
  def apply(left: Sentence, right: Sentence) = BinarySentence(left, right, Implies)
  
  def unapply(sentence: Sentence) = sentence match {
    case BinarySentence(left, right, Implies) => Some((left, right))
    case _ => None
  }

  override def toString() = "if"
}

case object Not extends UnaryConnective {
  def matches(op: Operator) = op match {
    case Not => true
    case _ => false
  }
  
  def evaluate(func: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(unary) => !func(unary)
    case _ => throw new IllegalArgumentException(args.toString)
  }
  
  def apply(operand: Sentence) = UnarySentence(operand, Not)
  
  def unapply(sentence: Sentence) = sentence match {
    case UnarySentence(s, Not) => Some(s)
    case _ => None
  }

  override def toString() = "not"
}
