package edu.osu.cse.groenkeb.logic

object Sentences {
  def atom(str: String) = AtomicSentence(Atom.parse(str));
  
  def and(left: Sentence, right: Sentence) = And(left, right)
  
  def or(left: Sentence, right: Sentence) = Or(left, right)
  
  def not(s: Sentence) = Not(s)
  
  def nil() = NullSentence
  
  def absurdity = Absurdity
}