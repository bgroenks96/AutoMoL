package edu.osu.cse.groenkeb.logic

trait Operator {
  def matches(op: Operator): Boolean
  override def toString(): String
}

trait Predicate extends Operator

trait Connective extends Operator {
  def evaluate(functor: Sentence => Boolean, args: Sentence*): Boolean
}

abstract class UnaryConnective extends Connective
abstract class BinaryConnective extends Connective

abstract class NamedPredicate(val name: String) extends Predicate
case class PropPredicate(pname: String) extends NamedPredicate(pname) {
  def matches(op: Operator) = op match {
    case PropPredicate(this.pname) => true
    case _ => false
  }
}
case class ObjectPredicate(pname: String, val args: Term*) extends NamedPredicate(pname) {
  def matches(op: Operator) = op match {
    case ObjectPredicate(this.pname, args@_*) if args.equals(this.args) => true
    case _ => false
  }
}

case class NullOp() extends Operator {
  def matches(op: Operator) = op match {
    case NullOp() => true
    case _ => false
  }
  
  override def toString() = ""
}
