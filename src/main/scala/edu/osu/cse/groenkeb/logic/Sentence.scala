package edu.osu.cse.groenkeb.logic

import edu.osu.cse.groenkeb.logic.parse.OperatorMatcher;

abstract class Sentence
{
  def matches(s: Sentence): Boolean
  def relate = SentenceRelation(this)
  override def toString: String
}

case class AtomicSentence(atom: Atom) extends Sentence
{
  def matches(s: Sentence) = s match {
    case AtomicSentence(atom) => this.atom.matches(atom)
    case _ => false
  }
  
  override def toString() = atom.toString()
}

case class BinarySentence(left: Sentence, right: Sentence, op: BinaryOperator) extends Sentence
{
  def matches(s: Sentence) = s match {
    case BinarySentence(left, right, op) => this.left.matches(left) && this.right.matches(right) && this.op.matches(op)
    case _ => false
  }
  
  override def toString() = String.format("%s(%s, %s)", op, left, right)
}

case class UnarySentence(s: Sentence, op: UnaryOperator) extends Sentence
{
  def matches(s: Sentence) = s match {
    case UnarySentence(s, op) => this.s.matches(s) && this.op.matches(op)
    case _ => false
  }
  
  override def toString() = String.format("%s%s", op, s)
}

case class NullSentence() extends Sentence
{
  def matches(s: Sentence) = s match {
    case NullSentence() => true
    case _ => false
  }
  
  override def toString() = ""
}
