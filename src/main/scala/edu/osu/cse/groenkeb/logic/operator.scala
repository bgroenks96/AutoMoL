package edu.osu.cse.groenkeb.logic

trait Operator[TResult] {
  def toRelation(s: Sentence*): ObjectRelation
  def matches[T](op: Operator[T]): Boolean
  def evaluate(functor: Sentence => TResult, args: Sentence*): TResult
  override def toString(): String
}

abstract class BinaryOperator[TResult] extends Operator[TResult]
abstract class UnaryOperator[TResult] extends Operator[TResult]

trait Predicate extends Operator[Boolean]
abstract class BinaryPredicate extends BinaryOperator[Boolean] with Predicate
abstract class UnaryPredicate extends UnaryOperator[Boolean] with Predicate

case class NullOp() extends Operator[Unit] {
  def toRelation(s: Sentence*) = s match { case Nil => NullObject() }
  
  def matches[T](op: Operator[T]) = op match {
    case NullOp() => true
    case _ => false
  }
  
  def evaluate(functor: Sentence => Unit, args: Sentence*) = Unit
  
  override def toString() = ""
}
