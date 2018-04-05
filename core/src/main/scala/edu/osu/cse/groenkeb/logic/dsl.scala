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
  }
}