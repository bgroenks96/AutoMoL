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
    def complexity = dsl.complexity(s)
    def weightedComplexity = dsl.weightedComplexity(s)
  }
  
  private def parityOf(sub: Sentence, wrt: Sentence, parity: Parity = Positive): Parity = {
    require(wrt.contains(sub))
    if (parity == Mixed) parity
    else wrt match {
      case s if s.matches(sub) => parity
      case Not(ne) if ne.matches(sub) => ~parity
      case Not(ne) => parityOf(sub, ne, ~parity)
      case If(ante, _) if ante.contains(sub) => parityOf(sub, ante, ~parity)
      case s => s.decompose.collect { case d if d.contains(sub) => parityOf(sub, d, parity) }.reduce((p1, p2) => p1 + p2)
    }
  }
  
  private def complexity(s: Sentence): Int = s match {
    case Absurdity|AtomicSentence(_) => 0
    case s => 1 + s.decompose.map(s => complexity(s)).sum
  }
  
  private def weightedComplexity(s: Sentence): Int = s match {
    case Absurdity|AtomicSentence(_) => 0
    case And(left, right) => 1 + weightedComplexity(left) + weightedComplexity(right)
    case Or(left, right) => 2 + weightedComplexity(left) + weightedComplexity(right)
    case Not(ne) => 3 + weightedComplexity(ne)
    case If(left, right) => 4 + weightedComplexity(left) + weightedComplexity(right)
    case QuantifiedSentence(s,_) => 5 + weightedComplexity(s)
  }
}