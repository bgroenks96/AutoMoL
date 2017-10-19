package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.Domain
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.proof.CompleteProof

abstract class VerificationRule extends AbstractRule

case class NegationVerification() extends VerificationRule() {  
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(Absurdity, _, _), _) => true
    case _ => false
  }
  
  def yields(conc: Sentence) = conc match {
    case UnarySentence(_, Not()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case UnarySentence(sentence, Not()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(Absurdity, _, _), prems)) if  exists(sentence).in(prems) =>
        val discharge = Assumption(sentence)
        CompleteResult(CompleteProof(Conclusion(conc, this, args), prems - discharge))
      case _ => IncompleteResult(UnaryParams(RelevantProof(Absurdity, Required(Assumption(sentence)))))
    }
    case _ => NullResult()
  }
  
  override def toString = "~V"
}

case class AndVerification() extends VerificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(s,_,_), _) if s != Absurdity => true
    case _ => false
  }
  
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
  
  override def toString = "&V"
}

case class OrVerification() extends VerificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(s,_,_), _) if s != Absurdity => true
    case _ => false
  }
  
  def yields(conc: Sentence) = conc match {
    case BinarySentence(_, _, Or()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case BinarySentence(left, right, Or()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(c, _, _), prems)) if c.matches(left) || c.matches(right) =>
        CompleteResult(CompleteProof(Conclusion(conc, this, args), prems))
      case _ => IncompleteResult(OptionParams(UnaryParams(AnyProof(left)),
                                              UnaryParams(AnyProof(right))))
    }
    case _ => NullResult()
  }
  
  override def toString = "+V"
}

case class ConditionalVerification() extends VerificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_,_,_), _) => true
    case _ => false
  }
  
  def yields(conc: Sentence) = conc match {
    case BinarySentence(_, _, Implies()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case BinarySentence(ante, conseq, Implies()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(`conseq`, _, _), prems)) =>
        CompleteResult(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), this, args), prems))
      case UnaryArgs(CompleteProof(Conclusion(Absurdity, _, _), prems)) if exists(ante).in(prems) =>
        val discharge = Assumption(ante)
        CompleteResult(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), this, args), prems - discharge))
      case _ => IncompleteResult(OptionParams(UnaryParams(AnyProof(conseq)),
                                              UnaryParams(RelevantProof(Absurdity, Required(Assumption(ante)), Assumption(BinarySentence(ante, conseq, Implies()))))))
    }
    case _ => NullResult()
  }
  
  override def toString = ">V"
}

case class UniversalVerification(domain: Domain) extends VerificationRule() {
  def major(proof: Proof) = true
  
  def yields(conc: Sentence) = conc match {
    case QuantifiedSentence(_, UniversalQuantifier(_)) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case QuantifiedSentence(sentence, UniversalQuantifier(term)) => args match {
      case NArgs(proofs) if validate(proofs, sentence, term) =>
        CompleteResult(CompleteProof(Conclusion(conc, this, args), proofs.flatMap { p => p.premises }.toSet))
      case _ => IncompleteResult(NParams(this.domain.terms.toSeq.map { 
          t => AnyProof(sentence.substitute(term, t))
        }))
    }
    case _ => NullResult()
  }
  
  private def validate(proofs: Seq[Proof], sentence: Sentence, term: Term): Boolean = 
    proofs.length == this.domain.size && this.domain.terms.forall {
      t => proofs.exists { p => p.conclusion match {
        case Some(conc) => conc.sentence.matches(sentence.substitute(term, t))
        case None => false
      }}
  }
  
  override def toString = "UV"
}

case class ExistentialVerification(domain: Domain) extends VerificationRule() {
  def major(proof: Proof) = true
  
  def yields(conc: Sentence) = conc match {
    case QuantifiedSentence(_, ExistentialQuantifier(_)) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case QuantifiedSentence(sentence, ExistentialQuantifier(term)) => args match {
      case UnaryArgs(proof) if validate(proof, sentence, term) =>
        CompleteResult(CompleteProof(Conclusion(conc, this, args), proof.premises))
      case _ => IncompleteResult(OptionParams(this.domain.terms.toSeq.map {
        t => UnaryParams(AnyProof(sentence.substitute(term, t)))
      }:_*))
    }
    case _ => NullResult()
  }
  
  private def validate(proof: Proof, sentence: Sentence, term: Term): Boolean = proof.conclusion match {
    case Some(conc) => this.domain.terms.exists { t => sentence.substitute(term, t).matches(conc.sentence) }
    case None => false
  }
  
  override def toString = "EV"
}

