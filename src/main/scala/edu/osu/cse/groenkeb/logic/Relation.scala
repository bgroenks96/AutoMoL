package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
abstract class Relation

abstract class ObjectRelation(val sentences: Sentence*) extends Relation
{
  def count = sentences.length
  
  def head = sentences(0)
  
  def tail = sentences(count - 1)
}

/**
 * Base type for meta-linguistic relations between objects in the object-language.
 */
abstract class MetaRelation(val relations: ObjectRelation*) extends ObjectRelation

/**
 * Base type for all connective relations: (s1, s2) in C(s1, s2)
 * where C is some logical connective operator
 */
abstract class ConnectiveRelation(s1: Sentence, s2: Sentence) extends ObjectRelation(s1, s2)
{
  def result: SentenceRelation
  
  def sentenceResult = result.member
 
  def left = head
    
  def right = tail
}

/**
 * Identity relation for any singular sentence s: (s, s)
 */
case class SentenceRelation(val sentence: Sentence) extends ObjectRelation(sentence)
{
  def member = sentence
}

case class TruthRelation(r: SentenceRelation) extends MetaRelation(r)
case class AbsurdityRelation(r: SentenceRelation) extends MetaRelation(r)
case class TurnstileRelation(prem: SentenceRelation, conc: SentenceRelation) extends MetaRelation(prem, conc)

case class NotRelation(s: Sentence) extends ConnectiveRelation(s, s)
{
  def result = SentenceRelation(new UnarySentence(s, Not()))
}

case class AndRelation(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2)
{
  def result = SentenceRelation(new BinarySentence(s1, s2, And()))
}

case class OrRelation(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2)
{
  def result = SentenceRelation(new BinarySentence(s1, s2, Or()))
}

case class ImpliesRelation(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2)
{
  def result = SentenceRelation(new BinarySentence(s1, s2, Implies()))
}

case class NullRelation() extends Relation

