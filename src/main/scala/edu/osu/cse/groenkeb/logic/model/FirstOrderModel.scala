package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.NullSentence
import edu.osu.cse.groenkeb.logic.Predicate
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.UnarySentence

case class FirstOrderModel(val diagram: AtomicDiagram) extends Model {
  def domain = diagram.domain
  def verify(sentence: Sentence): Boolean = sentence match {
    case AtomicSentence(atom) => diagram.has(atom.toRelation)
    case UnarySentence(operand, conn) => conn.evaluate(verify, operand)
    case BinarySentence(left, right, conn) => conn.evaluate(verify, left, right)
    case NullSentence() => false
  }
}

object FirstOrderModel {
  def apply(sentences: Sentence*) = from(sentences:_*)
  def from(sentences: Sentence*): FirstOrderModel = new FirstOrderModel(diagram(extract(sentences:_*), sentences:_*))
  def empty = from()
  
  private def extract(sentences: Sentence*): Domain = sentences.toList match {
    case Nil => Domain()
    case AtomicSentence(atom) :: rem => Domain(atom.terms:_*) ++ extract(rem:_*)
    case sentence :: rem => extract(sentence.decompose():_*) ++ extract(rem:_*)
  }
  
  private def diagram(domain: Domain, sentences: Sentence*): AtomicDiagram = sentences.toList match {
    case Nil => AtomicDiagram(domain)
    case AtomicSentence(atom) :: rem => AtomicDiagram(domain, atom.toRelation) ++ diagram(domain, rem:_*)
    case _ => throw new IllegalArgumentException("found non-atomic sentence in model declaration")
    //case sentence :: rem => diagram(domain, sentence.decompose():_*) ++ diagram(domain, rem:_*)
  }
}