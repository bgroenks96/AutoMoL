package edu.osu.cse.groenkeb.logic

trait Operator {
  def matches(op: Operator): Boolean
  override def toString(): String
}

trait Quantifier extends Operator {
  def evaluate(domain: Domain, func: Sentence => Boolean, arg: Sentence): Boolean
}

trait Connective extends Operator {
  def evaluate(func: Sentence => Boolean, args: Sentence*): Boolean
}

abstract class UnaryConnective extends Connective {
  def apply(operand: Sentence): UnarySentence
  def unapply(sentence: Sentence): Option[Sentence]
}

abstract class BinaryConnective extends Connective {
  def apply(left: Sentence, right: Sentence): BinarySentence
  def unapply(sentence: Sentence): Option[(Sentence, Sentence)]
}

sealed abstract class Predicate extends Operator
case class NamedPredicate(val name: String) extends Predicate {
  require(name != IdentityPredicate.name)
  def matches(op: Operator) = op match {
    case NamedPredicate(this.name) => true
    case _ => false
  }
  
  override def toString = name
}
case class IdentityPredicate() extends Predicate {
  def matches(op: Operator) = op match {
    case IdentityPredicate() => true
    case _ => false
  }
  
  override def toString = IdentityPredicate.name
}

object IdentityPredicate {
  def name = "I"
}

case object NullOp extends Operator {
  def matches(op: Operator) = op match {
    case NullOp => true
    case _ => false
  }
  
  override def toString() = ""
}
