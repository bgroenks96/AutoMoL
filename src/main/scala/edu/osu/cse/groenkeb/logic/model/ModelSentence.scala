package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Atom
import edu.osu.cse.groenkeb.logic.Sentence

case class ModelSentence(predicate: AtomicPredicate, atoms: Atom*) extends Sentence {
  def matches(sentence: Sentence) = sentence match {
    case ModelSentence(this.predicate, this.atoms) => true
    case _ => false
  }
  
  def toRelation = AtomicRelation(predicate.name, atoms:_*)
}