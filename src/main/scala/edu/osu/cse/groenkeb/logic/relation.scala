package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
abstract class Relation
case class ObjectRelation(val predicate: Predicate, val terms: Term*) {
  override def toString = String.format("%s[%s]", predicate, terms.mkString(","))
}
