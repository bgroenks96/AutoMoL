package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.Not
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.types.Assumption
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.Or
import edu.osu.cse.groenkeb.logic.Implies

abstract class VerificationRule extends AbstractRule {
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false
  }
}

case class NegationVerification() extends VerificationRule() {  
  override def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(Absurdity(), _, _), _) => true
    case _ => false
  }
  
  def yields(conc: Sentence) = conc match {
    case UnarySentence(_, Not()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case UnarySentence(sentence, Not()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(Absurdity(), _, _), prems)) if  exists(sentence).in(prems) =>
        CompleteResult(CompleteProof(Conclusion(sentence, this, args), prems))
      case _ => IncompleteResult(UnaryParams(RelevantProof(Absurdity(), Required(Assumption(sentence)))))
    }
    case _ => NullResult()
  }
}

case class AndVerification() extends VerificationRule() {
  def yields(conc: Sentence) = conc match {
    case BinarySentence(_, _, And()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case BinarySentence(left, right, And()) => args match {
      case BinaryArgs(CompleteProof(Conclusion(`left`, _, _), pleft), CompleteProof(Conclusion(`right`, _, _), pright)) =>
        CompleteResult(CompleteProof(Conclusion(conc, this, args), pleft ++ pright))
      case _ => IncompleteResult(BinaryParams(AnyProof(left), AnyProof(right)))
    }
    case _ => NullResult()
  }
}

case class OrVerification() extends VerificationRule() {
  def yields(conc: Sentence) = conc match {
    case BinarySentence(_, _, Or()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case BinarySentence(left, right, Or()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(`left`, _, _), pleft)) =>
        CompleteResult(CompleteProof(Conclusion(conc, this, args), pleft))
      case _ => IncompleteResult(OptionParams(UnaryParams(AnyProof(left)),
                                              UnaryParams(AnyProof(right))))
    }
    case _ => NullResult()
  }
}

case class ConditionalVerification() extends VerificationRule() {
  def yields(conc: Sentence) = conc match {
    case BinarySentence(_, _, Implies()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case BinarySentence(ante, conseq, Implies()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(`conseq`, _, _), prems)) =>
        CompleteResult(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), this, args), prems))
      case UnaryArgs(CompleteProof(Conclusion(Absurdity(), _, _), prems)) if exists(ante).in(prems) =>
        CompleteResult(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), this, args), prems))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
}

