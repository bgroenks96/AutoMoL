package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Atom
import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.SentenceRelation

case class AtomicRelation(val name: String, val atoms: Atom*) extends ObjectRelation(atoms.map { a => AtomicSentence(a) }:_*) {
  def toSentence = atoms match {
    case Seq(a1) => ModelSentence(AtomicPredicate(name), a1)
    case Seq(a1, a2) => ModelSentence(AtomicPredicate(name), a1, a2)
  }
  
  def contains(relation: ObjectRelation) = relation match {
    case SentenceRelation(s) if atoms.exists { a => s.matches(AtomicSentence(a)) } => true
    case _ => false
  }
  
  def decompose = atoms.map { a => SentenceRelation(AtomicSentence(a)) }.toList
}