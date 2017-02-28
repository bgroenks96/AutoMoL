package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Predicate
import edu.osu.cse.groenkeb.logic.Operator
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.AtomicSentence

case class AtomicPredicate(name: String) extends Predicate {
  def matches[T](op: Operator[T]): Boolean = op match {
    case AtomicPredicate(this.name) => true
    case _ => false
  }
  
  def toRelation(sentences: Sentence*) = sentences match {
    case Seq(AtomicSentence(a)) => AtomicRelation(name, a)
    case Seq(AtomicSentence(a1), AtomicSentence(a2)) => AtomicRelation(name, a1, a2)
    case _ => throw new IllegalArgumentException(sentences.toString)
  }
  
  def evaluate(functor: Sentence => Boolean, args: Sentence*) = args match {
    case Seq(s) if s.isInstanceOf[ModelSentence] => functor(s)
  }
}