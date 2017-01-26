package edu.osu.cse.groenkeb.logic

abstract class Sentence
{
  def matches(s: Sentence): Boolean
  override def toString(): String
}

case class AtomicSentence(atom: Atom) extends Sentence
{
  def matches(s: Sentence) = s match {
    case AtomicSentence(atom) => true
    case _ => false
  }
  
  override def toString() = atom.toString()
}

case class BinarySentence(left: Sentence, right: Sentence, op: BinaryOperator) extends Sentence
{
  def matches(s: Sentence) = s match {
    case BinarySentence(left, right, op) => true
    case _ => false
  }
  
  override def toString() = String.format("%s(%s, %s)", op, left, right)
}

case class UnarySentence(s: Sentence, op: UnaryOperator) extends Sentence
{
  def matches(s: Sentence) = s match {
    case UnarySentence(s, op) => true
    case _ => false
  }
  
  override def toString() = String.format("%s(%s)", op, s)
}

case class NullSentence() extends Sentence
{
  def matches(s: Sentence) = s match {
    case NullSentence() => true
    case _ => false
  }
  
  override def toString() = ""
}
