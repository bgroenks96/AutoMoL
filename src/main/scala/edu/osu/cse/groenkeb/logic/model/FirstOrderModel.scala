package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.parse.ParserException

case class FirstOrderModel(val diagram: AtomicDiagram) extends Model {
  def domain = diagram.domain
  def withDomain(domain: Domain) = FirstOrderModel(AtomicDiagram(domain.merge(this.domain), diagram.relations:_*))
  def verify(sentence: Sentence): Boolean = sentence match {
    case AtomicSentence(atom) => diagram.has(atom.toRelation)
    case UnarySentence(operand, conn) => conn.evaluate(verify, operand)
    case BinarySentence(left, right, conn) => conn.evaluate(verify, left, right)
    case QuantifiedSentence(operand, quantifier) => quantifier.evaluate(diagram.domain, verify, operand)
    case NullSentence() => false
  }
}

object FirstOrderModel {
  def apply(sentences: Sentence*) = from(sentences:_*)
  def from(sentences: Sentence*): FirstOrderModel = new FirstOrderModel(diagram(sentences:_*))
  def empty = from()
  
//  private def extract(sentences: Sentence*): Domain = sentences.toList match {
//    case Nil => Domain()
//    case AtomicSentence(atom) :: rem => Domain(atom.terms:_*) ++ extract(rem:_*)
//    case sentence :: rem => extract(sentence.decompose():_*) ++ extract(rem:_*)
//  }
  
  private def diagram(sentences: Sentence*): AtomicDiagram = sentences.toList match {
    case Nil => AtomicDiagram(Domain())
    case AtomicSentence(atom) :: rem => AtomicDiagram(Domain(atom.terms:_*), atom.toRelation) ++ diagram(rem:_*)
    case _ => throw ParserException("found non-atomic sentence in model declaration")
    //case sentence :: rem => diagram(domain, sentence.decompose():_*) ++ diagram(domain, rem:_*)
  }
}