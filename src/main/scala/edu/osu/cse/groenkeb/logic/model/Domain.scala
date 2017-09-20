package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Atom
import edu.osu.cse.groenkeb.logic.Term

import scala.collection.immutable.Set

case class Domain(terms: Set[Term]) {
  def this() = this(Set())
  def +(term: Term) = withMember(term)
  def ++(domain: Domain) = merge(domain)
  def withMember(term: Term) = if (lacks(term)) Domain(this.terms + term); else this
  def merge(domain: Domain) = Domain(domain.terms.union(this.terms))
  def has(term: Term) = terms.contains(term)
  def lacks(term: Term) = !has(term)
  def size = this.terms.size
  override def toString = String.format("%s(%s)", getClass.getSimpleName, terms.mkString(","))
}

object Domain {
  def apply() = new Domain(Set())
  def apply(terms: scala.collection.Iterable[Term]) = new Domain(terms.toSet)
  def apply(terms: Term*) = new Domain(terms.toSet)
}
