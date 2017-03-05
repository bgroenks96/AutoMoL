package edu.osu.cse.groenkeb.logic

abstract class Sentence
{
  def matches(s: Sentence): Boolean
  def contains(s: Sentence): Boolean
  def decompose(): Seq[Sentence]
  override def toString: String
}

case class AtomicSentence(atom: Atom) extends Sentence
{
  def matches(s: Sentence) = s match {
    case AtomicSentence(atom) => this.atom.matches(atom)
    case _ => false
  }
  
  def contains(s: Sentence) = matches(s)
  
  def decompose() = List(this)
  
  override def toString() = atom.toString()
}

case class BinarySentence(val left: Sentence, val right: Sentence, val conn: BinaryConnective) extends Sentence
{
  def matches(s: Sentence) = s match {
    case BinarySentence(left, right, conn) => this.left.matches(left) && this.right.matches(right) && this.conn.matches(conn)
    case _ => false
  }
  
  def contains(s: Sentence) = left.contains(s) || right.contains(s)
  
  def decompose() = List(left, right)
  
  override def toString() = String.format("%s(%s.%s)", conn, left, right)
}

case class UnarySentence(val operand: Sentence, conn: UnaryConnective) extends Sentence
{
  def matches(s: Sentence) = s match {
    case UnarySentence(operand, conn) => this.operand.matches(operand) && this.conn.matches(conn)
    case _ => false
  }
  
  def contains(s: Sentence) = operand.contains(s)
  
  def decompose() = List(operand)
  
  override def toString() = String.format("%s(%s)", conn, operand)
}

case class NullSentence() extends Sentence
{
  def matches(s: Sentence) = s match {
    case NullSentence() => true
    case _ => false
  }
  
  def contains(s: Sentence) = false
  
  def decompose() = Nil
  
  override def toString() = ""
}
