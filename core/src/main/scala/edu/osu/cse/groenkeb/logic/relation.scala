package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
sealed abstract class Relation(val predicate: Predicate, val rank: Int)
case class ObjectRelation(p: Predicate, terms: Term*) extends Relation(p, terms.length) {
  override def toString = String.format("%s[%s]", predicate, terms.mkString(","))
}
case class EmptyRelation(p: Predicate, r: Int) extends Relation(p, r)
