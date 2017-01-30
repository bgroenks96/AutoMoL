package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
abstract class Relation(s1: Sentence, s2: Sentence)

case class AtomicRelation(s: AtomicSentence) extends Relation(s, s)
case class TruthRelation(s: Sentence) extends Relation(s, s)
case class NotRelation(s: Sentence) extends Relation(s, s)
case class AndRelation(s1: Sentence, s2: Sentence) extends Relation(s1, s2)
case class OrRelation(s1: Sentence, s2: Sentence) extends Relation(s1, s2)
case class ImpliesRelation(s1: Sentence, s2: Sentence) extends Relation(s1, s2)
case class TurnstileRelation(s1: Sentence, s2: Sentence) extends Relation(s1, s2)
