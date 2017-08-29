package edu.osu.cse.groenkeb.logic

sealed abstract class Sentence
{
  def matches(s: Sentence): Boolean
  def contains(s: Sentence): Boolean
  def substitute(orig: Term, sub: Term): Sentence
  def decompose(): Seq[Sentence]
  override def toString: String
  
  def isAbsurdity = this.isInstanceOf[Absurdity]
  
  def isNotAbsurdity = !this.isAbsurdity
}

case class AtomicSentence(atom: Atom) extends Sentence
{
  def matches(s: Sentence) = s match {
    case AtomicSentence(atom) => this.atom.matches(atom)
    case _ => false
  }
  
  def contains(s: Sentence) = matches(s)
  
  def decompose() = List(this)
  
  def toRelation = atom.toRelation
  
  def substitute(orig: Term, sub: Term) = AtomicSentence(atom.substitute(orig, sub))
  
  override def toString() = atom.toString()
}

case class BinarySentence(val left: Sentence, val right: Sentence, val conn: BinaryConnective) extends Sentence
{
  require(left != Absurdity() && right != Absurdity())
  def matches(s: Sentence) = s match {
    case BinarySentence(left, right, conn) => this.left.matches(left) && this.right.matches(right) && this.conn.matches(conn)
    case _ => false
  }
  
  def contains(s: Sentence) = matches(s) || left.contains(s) || right.contains(s)
  
  def substitute(orig: Term, sub: Term) = BinarySentence(left.substitute(orig, sub), right.substitute(orig, sub), conn)
  
  def decompose() = List(left, right)
  
  override def toString() = String.format("%s(%s,%s)", conn, left, right)
}

case class UnarySentence(val operand: Sentence, val conn: UnaryConnective) extends Sentence
{
  require(operand != Absurdity())
  def matches(s: Sentence) = s match {
    case UnarySentence(operand, conn) => this.operand.matches(operand) && this.conn.matches(conn)
    case _ => false
  }
  
  def contains(s: Sentence) = matches(s) || operand.contains(s)
  
  def substitute(orig: Term, sub: Term) = UnarySentence(operand.substitute(orig, sub), conn)
  
  def decompose() = List(operand)
  
  override def toString() = String.format("%s(%s)", conn, operand)
}

case class QuantifiedSentence(val operand: Sentence, val quantifier: Quantifier) extends Sentence
{
    require(operand != Absurdity())
    def matches(s: Sentence) = s match {
    case QuantifiedSentence(operand, quantifier) => this.operand.matches(operand) && this.quantifier.matches(quantifier)
    case _ => false
  }
  
  def contains(s: Sentence) = matches(s) || operand.contains(s)
  
  def substitute(orig: Term, sub: Term) = QuantifiedSentence(operand.substitute(orig, sub), quantifier)
  
  def decompose() = List(operand)
  
  override def toString() = String.format("%s(%s)", quantifier, operand)
}

case class Absurdity() extends Sentence
{
  def matches(s: Sentence) = s match {
    case Absurdity() => true
    case _ => false
  }
  
  def contains(s: Sentence) = matches(s)
  
  def decompose() = List(this)
  
  def substitute(orig: Term, sub: Term) = this
  
  override def toString() = "!"
}

case class NullSentence() extends Sentence
{
  def matches(s: Sentence) = s match {
    case NullSentence() => true
    case _ => false
  }
  
  def contains(s: Sentence) = false
  
  def substitute(orig: Term, sub: Term) = this
  
  def decompose() = Nil
  
  override def toString() = ""
}
