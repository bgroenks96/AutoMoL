package edu.osu.cse.groenkeb.logic

object dsl {
  implicit def booleanExtensions(b: Boolean) = new {
    def and(arg: Boolean) = b && arg
    def or(arg: Boolean) = b || arg
    def not = !b
  }
  
  implicit def sentenceExtensions(s: Sentence) = new {
    def is(arg: Sentence) = s == arg
    def is(arg: (Sentence, Sentence)) = (s == arg._1) or (s == arg._2)
    def has(op: Operator) = s match {
      case UnarySentence(s, `op`) => true
      case BinarySentence(s1, s2, `op`) => true
      case _ => false
    }
    def or(arg: Sentence) = (s, arg)
    def isAtomic = s.isInstanceOf[AtomicSentence] or (s == Absurdity)
    def parity(sub: Sentence): Parity = parityOf(sub, s)
    def accessible(sub: Sentence): Boolean = s.contains(sub) and (parityOf(sub, s) == Positive)
  }
  
  private def parityOf(sub: Sentence, wrt: Sentence, parity: Parity = Positive): Parity = {
    require(wrt.contains(sub))
    wrt match {
      case s if s.matches(sub) => parity
      case Not(s) if s.matches(sub) => ~parity
      case Not(s) => parityOf(sub, s, ~parity)
      case If(ante, _) if ante.contains(sub) => parityOf(sub, ante, ~parity)
      case BinarySentence(left, right, _) if left.contains(sub) => parityOf(sub, left, parity)
      case BinarySentence(left, right, _) if right.contains(sub) => parityOf(sub, right, parity)
      case UnarySentence(s, _) if s.contains(sub) => parityOf(sub, s, parity)
    }
  }
}