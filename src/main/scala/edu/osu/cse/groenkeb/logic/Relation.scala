package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
abstract class Relation(s1: Sentence, s2: Sentence)

abstract class ConnectiveRelation(s1: Sentence, s2: Sentence) extends Relation(s1, s2)
{
  def result(): Sentence
}

case class ContingentRelation(s: Sentence) extends Relation(s, s)
case class TruthRelation(s: Sentence) extends Relation(s, s)
case class TurnstileRelation(s1: Sentence, s2: Sentence) extends Relation(s1, s2)

case class NotRelation(s: Sentence) extends ConnectiveRelation(s, s)
{
  def result() = new UnarySentence(s, Not())
}

case class AndRelation(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2)
{
  def result() = new BinarySentence(s1, s2, And())
}

case class OrRelation(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2)
{
  def result() = new BinarySentence(s1, s2, Or())
}

case class ImpliesRelation(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2)
{
  def result()= new BinarySentence(s1, s2, Implies())
}
