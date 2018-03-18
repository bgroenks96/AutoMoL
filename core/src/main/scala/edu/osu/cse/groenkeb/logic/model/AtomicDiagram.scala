package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Domain;
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Term
import edu.osu.cse.groenkeb.logic.Atom

case class AtomicDiagram(val domain: Domain, val relations: ObjectRelation*) {
  validate()
  def ++(diagram: AtomicDiagram) = merge(diagram)
  def merge(diagram: AtomicDiagram) = AtomicDiagram(this.domain ++ diagram.domain, this.relations.union(diagram.relations).distinct:_*)
  def has(relation: ObjectRelation): Boolean = relations.contains(relation)  
  def has(term: Term): Boolean = domain.has(term)
  def validate(atom: Atom): Boolean = this.relations.exists { r => r.predicate.matches(atom.predicate) } &&
                                      atom.terms.forall { t => domain.has(t) }
  override def toString = String.format("%s(%s Relations{%s})", getClass.getSimpleName, domain, relations.mkString(", "))
  
  private def validate() {
    relations.find { r1 => relations.exists { 
      r2 => r1.predicate.matches(r2.predicate) && r1.terms.length != r2.terms.length
    }}.foreach { r => throw ModelException("incompatible definitions of predicate " + r.predicate) }
  }
}
