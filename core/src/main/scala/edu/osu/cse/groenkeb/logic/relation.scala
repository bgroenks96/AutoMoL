package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
abstract class Relation(val predicate: Predicate, val rank: Int)
case class ObjectRelation(private val p: Predicate, val terms: Term*) extends Relation(p, terms.length) {
  override def toString = String.format("%s[%s]", predicate, terms.mkString(","))
}
case class EmptyRelation(private val p: Predicate, private val r: Int) extends Relation(p, r)
