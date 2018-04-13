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
    def accessible(atom: Atom): Boolean = checkAccessibility(atom, s)
  }
  
  private def checkAccessibility(atom: Atom, sentence: Sentence): Boolean = sentence match {
    case Not(s) => false
    case If(_, right) if right.contains(AtomicSentence(atom)) => true
    case If(left, _) => false
    case s => s.contains(AtomicSentence(atom))
  }
}