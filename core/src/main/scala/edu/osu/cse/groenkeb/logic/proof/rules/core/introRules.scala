package edu.osu.cse.groenkeb.logic.proof.rules.core

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._

case object NegationIntroduction extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(conclusion: Sentence) = conclusion match {
    case Not(_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence] = None)(implicit context: ProofContext) = goal match {
    case Not(sentence) if major == None => Some(UnaryParams(RelevantProof(Absurdity, Required(Assumption(sentence, bind)))))
    case _ => None
  }

  def infer(args: RuleArgs)(implicit context: ProofContext) = goal match {
    case Not(sentence) => args match {
      case UnaryArgs(disproof@Proof(Absurdity,_,_, assumptions,_)) if disproof uses sentence =>
        Some(Proof(goal, this, args, assumptions.discharge(sentence), bind))
      case _ => None
    }
    case _ => None
  }
  
  override def toString = "~I"
}

case object AndIntroduction extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(conclusion: Sentence) = conclusion match {
    case And(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence] = None)(implicit context: ProofContext) = goal match {
    case And(left, right) if major == None => Some(BinaryParams(AnyProof(left), AnyProof(right)))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case BinaryArgs(Proof(left, _, _, pleft, _), Proof(right, _, _, pright, _)) => Some(Proof(goal, this, args, pleft ++ pright))
    case _ => None
  }
  
  override def toString = "&I"
}

case object OrIntroduction extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(conc: Sentence) = conc match {
    case Or(_,_) => true
    case _ => false
  }
  
  def params(major: Option[Sentence] = None)(implicit context: ProofContext) = goal match {
    case Or(left, right) if major == None =>
      Some(OptionParams(
        UnaryParams(AnyProof(left)),
        UnaryParams(AnyProof(right))))
    case _ => None
  }

  def infer(args: RuleArgs)(implicit context: ProofContext) = goal match {
    case Or(left, right) => args match {
      case UnaryArgs(Proof(c, _, _, prems, _)) if c.matches(left) || c.matches(right) =>
        Some(Proof(goal, this, args, prems))
      case _ => None
    }
    case _ => None
  }
  
  override def toString = "vI"
}

case object IfIntroduction extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(conc: Sentence) = conc match {
    case If(_,_) => true
    case _ => false
  }

  def params(major: Option[Sentence] = None)(implicit context: ProofContext) = goal match {
    case If(ante, conseq) if major == None =>
      Some(OptionParams(
        UnaryParams(AnyProof(conseq)),
        UnaryParams(RelevantProof(conseq, Vacuous(Assumption(ante, bind)), Assumption(If(ante, conseq))))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = goal match {
    case If(ante, conseq) => args match {
      case UnaryArgs(anteDisproof@Proof(Absurdity, _, _, assumptions, _)) if anteDisproof uses ante =>
        Some(Proof(If(ante, conseq), this, args, assumptions.discharge(ante), bind))
      case UnaryArgs(consProof@Proof(`conseq`, _, _, assumptions, _)) if consProof uses ante =>
        Some(Proof(If(ante, conseq), this, args, assumptions.discharge(ante), bind))
      case UnaryArgs(Proof(`conseq`, _, _, assumptions, _)) =>
        Some(Proof(If(ante, conseq), this, args, assumptions))
      case _ => None
    }
    case _ => None
  }
  
  override def toString = ">I"
}
