package edu.osu.cse.groenkeb.logic

/**
 * Relational ordered pair (s1, s2)
 */
sealed abstract class Relation

sealed abstract class ObjectRelation(val sentences: Sentence*) extends Relation {
  def result: SentenceRelation

  def sentenceResult = result.sentence

  def count = sentences.length

  def head = sentences(0)

  def tail = sentences(count - 1)

  def decompose(): List[ObjectRelation]

  def contains(r: ObjectRelation): Boolean

  override def toString() = String.format("%s: (%s)", this.getClass.getSimpleName, sentences.mkString(", "))
}

/**
 * Base type for meta-linguistic relations between sentences in the object-language.
 */
sealed abstract class MetaRelation extends Relation

/**
 * Base type for all connective relations: (s1, s2) in C(s1, s2)
 * where C is some logical connective operator
 */
sealed abstract class ConnectiveRelation(s1: Sentence, s2: Sentence) extends ObjectRelation(s1, s2) {
  def left = head

  def right = tail

  def decompose() = List(s1.toRelation, s2.toRelation)

  // first check if connective sentence matches, then left sentence, then right sentence iff not equal to left
  def contains(r: ObjectRelation) = sentenceResult.matches(r.sentenceResult) || s1.toRelation.contains(r) || (!s1.matches(s2) && s2.toRelation.contains(r))
}

/**
 * Identity relation for any singular sentence s: (s, s)
 */
case class SentenceRelation(val sentence: Sentence) extends ObjectRelation(sentence) {
  def result = this

  def decompose() = List(sentence.toRelation)

  def contains(r: ObjectRelation) = sentence.matches(r.sentenceResult) || (decompose() match {
    case SentenceRelation(s) :: Nil => s match {
      case AtomicSentence(a) => s.matches(r.sentenceResult)
      case complexSentence => s.toRelation.contains(r)
    }
    case complexRelation :: Nil => complexRelation.contains(r)
  })
  
  def atomic = sentence match {
    case AtomicSentence(_) => true
    case _ => false
  }
}

case class Absurdity() extends ObjectRelation(Sentences.absurdity()) {
  def sentence = Sentences.absurdity()

  def result = SentenceRelation(sentence)

  def decompose() = List()

  def contains(r: ObjectRelation) = false
}

// ----- META RELATIONS ------ //

case class Turnstile(val premises: Seq[Sentence], val conclusion: Sentence) extends MetaRelation

// ----- CONNECTIVE RELATIONS ----- //

case class Not(s: Sentence) extends ConnectiveRelation(s, s) {
  def result = SentenceRelation(UnarySentence(s, NotOp()))
}

case class And(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2) {
  def result = SentenceRelation(BinarySentence(s1, s2, AndOp()))
}

case class Or(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2) {
  def result = SentenceRelation(BinarySentence(s1, s2, OrOp()))
}

case class Implies(s1: Sentence, s2: Sentence) extends ConnectiveRelation(s1, s2) {
  def result = SentenceRelation(BinarySentence(s1, s2, ImpliesOp()))
}

// -------------------------------- //

case class NullObject() extends ObjectRelation(NullSentence(), NullSentence()) {
  def result = SentenceRelation(Sentences.nil())

  def decompose() = List()

  def contains(r: ObjectRelation) = false
}

