package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._
import edu.osu.cse.groenkeb.logic.model._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._

case class ModelRule(val model: FirstOrderModel) extends BaseRule {
  def major(sentence: Sentence) = sentence.isInstanceOf[AtomicSentence]
  
  def yields(conc: Sentence) = conc match {
    case AtomicSentence(atom) => model.verify(conc)
    case Absurdity => true
    case _ => false
  }
  
  def params(major: Option[Sentence] = None)(implicit context: ProofContext) = goal match {
    case s:AtomicSentence if major == None and model.verify(s) => Some(EmptyParams)
    case Absurdity => major match {
      case Some(s:AtomicSentence) if !model.verify(s) => Some(UnaryParams(EmptyProof(s)))
      case _ => None
    }
    case _ => None
  }

  def infer(args: RuleArgs)(implicit context: ProofContext) = goal match {
    case AtomicSentence(atom) =>
      if (!model.validate(atom)) throw new IllegalArgumentException(String.format("%s is not defined in the model", atom))
      args match {
        case EmptyArgs if model.verify(goal) => Some(Proof(goal, ModelRule.this, args, Set()))
        case _ => None
      }
    case Absurdity =>
      args match {
        case UnaryArgs(Proof(s: AtomicSentence, IdentityRule, _, assumptions, _)) if !model.verify(s) =>
          if (!model.validate(s.atom)) throw new IllegalArgumentException(String.format("%s is not defined in the model", s.atom))
          Some(Proof(Absurdity, ModelRule.this, args, assumptions))
        case _ => None
      }
    case _ => None
  }

  override def toString = "M"
}
