package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Atom
import edu.osu.cse.groenkeb.logic.Term

case class Domain(val terms: Term*) {
  def :+(term: Term) = withMember(term)
  def ++(domain: Domain) = merge(domain)
  def withMember(term: Term) = if (lacks(term)) Domain((this.terms :+ term):_*); else this
  def merge(domain: Domain) = Domain(domain.terms.union(this.terms).distinct:_*)
  def has(term: Term) = terms.contains(term)
  def lacks(term: Term) = !has(term)
  override def toString = String.format("%s(%s)", getClass.getSimpleName, terms.mkString(","))
}