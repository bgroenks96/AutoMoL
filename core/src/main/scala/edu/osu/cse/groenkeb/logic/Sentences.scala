package edu.osu.cse.groenkeb.logic

object Sentences {
  def atom(str: String) = AtomicSentence(Atom.parse(str));
  
  def and(left: Sentence, right: Sentence) = BinarySentence(left ,right, And());
  
  def or(left: Sentence, right: Sentence) = BinarySentence(left, right, Or());
  
  def not(s: Sentence) = UnarySentence(s, Not());
  
  def nil() = NullSentence();
  
  def absurdity = Absurdity
}