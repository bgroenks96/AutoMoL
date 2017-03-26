package edu.osu.cse.groenkeb.logic

import edu.osu.cse.groenkeb.logic.model.Domain

trait Operator {
  def matches(op: Operator): Boolean
  override def toString(): String
}

trait Predicate extends Operator

trait Quantifier extends Operator {
  def evaluate(domain: Domain, functor: Sentence => Boolean, arg: Sentence): Boolean
}

trait Connective extends Operator {
  def evaluate(functor: Sentence => Boolean, args: Sentence*): Boolean
}

abstract class UnaryConnective extends Connective
abstract class BinaryConnective extends Connective

case class NamedPredicate(val name: String) extends Predicate {
  def matches(op: Operator) = op match {
    case NamedPredicate(this.name) => true
    case _ => false
  }
  
  override def toString = name
}

case class NullOp() extends Operator {
  def matches(op: Operator) = op match {
    case NullOp() => true
    case _ => false
  }
  
  override def toString() = ""
}
