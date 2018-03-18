package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.model.rules.AndFalsification
import edu.osu.cse.groenkeb.logic.model.rules.AndVerification
import edu.osu.cse.groenkeb.logic.model.rules.ConditionalFalsification
import edu.osu.cse.groenkeb.logic.model.rules.ConditionalVerification
import edu.osu.cse.groenkeb.logic.model.rules.ExistentialFalsification
import edu.osu.cse.groenkeb.logic.model.rules.ExistentialVerification
import edu.osu.cse.groenkeb.logic.model.rules.ModelRule
import edu.osu.cse.groenkeb.logic.model.rules.NegationFalsification
import edu.osu.cse.groenkeb.logic.model.rules.NegationVerification
import edu.osu.cse.groenkeb.logic.model.rules.OrFalsification
import edu.osu.cse.groenkeb.logic.model.rules.OrVerification
import edu.osu.cse.groenkeb.logic.model.rules.UniversalFalsification
import edu.osu.cse.groenkeb.logic.model.rules.UniversalVerification
import edu.osu.cse.groenkeb.logic.parse.ParserException
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

case class FirstOrderModel(val diagram: AtomicDiagram) extends Model {
  def domain = diagram.domain
  def withDomain(domain: Domain) = FirstOrderModel(AtomicDiagram(domain.merge(this.domain), diagram.relations:_*))
  def validate(atom: Atom): Boolean = diagram.validate(atom)
  def verify(sentence: Sentence): Boolean = sentence match {
    case AtomicSentence(atom) => diagram.has(atom.toRelation)
    case UnarySentence(operand, conn) => conn.evaluate(verify, operand)
    case BinarySentence(left, right, conn) => conn.evaluate(verify, left, right)
    case QuantifiedSentence(operand, quantifier) => quantifier.evaluate(diagram.domain, verify, operand)
    case Absurdity => false
    case NullSentence => false
  }
}

object FirstOrderModel {
  def apply(sentences: Sentence*) = from(sentences: _*)
  def from(sentences: Sentence*): FirstOrderModel = new FirstOrderModel(diagram(sentences: _*))
  def empty = from()
  
//  private def extract(sentences: Sentence*): Domain = sentences.toList match {
//    case Nil => Domain()
//    case AtomicSentence(atom) :: rem => Domain(atom.terms:_*) ++ extract(rem:_*)
//    case sentence :: rem => extract(sentence.decompose():_*) ++ extract(rem:_*)
//  }
  
  private def diagram(sentences: Sentence*): AtomicDiagram = sentences.toList match {
    case Nil => AtomicDiagram(Domain())
    case AtomicSentence(atom) :: rem => AtomicDiagram(Domain(atom.terms.toSet), atom.toRelation) ++ diagram(rem:_*)
    case _ => throw ParserException("found non-atomic sentence in model declaration")
    //case sentence :: rem => diagram(domain, sentence.decompose():_*) ++ diagram(domain, rem:_*)
  }
}