package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Atom
import edu.osu.cse.groenkeb.logic.AtomicSentence

case class AtomicDiagram(val domain: Domain, val relations: AtomicRelation*) {
  def verify(sentence: AtomicRelation) = relations.contains(sentence)
  def has(atom: Atom) = domain.has(atom)
}