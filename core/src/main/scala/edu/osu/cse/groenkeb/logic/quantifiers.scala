package edu.osu.cse.groenkeb.logic

case class ExistentialQuantifier(term: Term) extends Quantifier {
  def matches(op: Operator) = op match {
    case ExistentialQuantifier(term) => true
    case _ => false
  }
  
  def evaluate(domain: Domain, functor: Sentence => Boolean, arg: Sentence) = {
    domain.terms exists { t => functor.apply(arg.substitute(term, t)) }
  }
  
  override def toString = "E:" + term.name
}

case object Exists {
  def apply(term: Term, sentence: Sentence) = QuantifiedSentence(sentence, ExistentialQuantifier(term))
  
  def unapply(sentence: Sentence) = sentence match {
    case QuantifiedSentence(sentence, ExistentialQuantifier(term)) => Some((term, sentence))
    case _ => None
  }
}

case class UniversalQuantifier(term: Term) extends Quantifier {
    def matches(op: Operator) = op match {
    case UniversalQuantifier(term) => true
    case _ => false
  }
  
  def evaluate(domain: Domain, functor: Sentence => Boolean, arg: Sentence) = {
    // forall returns true for any empty collection, so we need to check the empty condition first
    !domain.terms.isEmpty && (domain.terms forall { t => functor.apply(arg.substitute(term, t)) })
  }
  
  override def toString = "U:" + term.name
}

case object ForAll {
  def apply(term: Term, sentence: Sentence) = QuantifiedSentence(sentence, UniversalQuantifier(term))
  
  def unapply(sentence: Sentence) = sentence match {
    case QuantifiedSentence(sentence, UniversalQuantifier(term)) => Some((term, sentence))
    case _ => None
  }
}
