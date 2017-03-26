package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Term

case class AtomicDiagram(val domain: Domain, val relations: ObjectRelation*) {
  def ++(diagram: AtomicDiagram) = merge(diagram)
  def merge(diagram: AtomicDiagram) = AtomicDiagram(this.domain ++ diagram.domain, this.relations.union(diagram.relations).distinct:_*)
  def has(relation: ObjectRelation) = relations.contains(relation)  
  def has(term: Term) = domain.has(term)
  override def toString = String.format("%s(%s Relations{%s})", getClass.getSimpleName, domain, relations.mkString(", "))
}
