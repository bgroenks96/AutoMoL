package edu.osu.cse.groenkeb.logic.proof.rules.core

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.utils.Only

case object NegationElimination extends BaseRule {
  def major(sentence: Sentence) = sentence match {
    case Not(_) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = sentence == Absurdity
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(Not(sentence)) =>
      Some(BinaryParams(EmptyProof(Not(sentence)), RelevantProof(sentence, Vacuous(), Assumption(Not(sentence)))))
    case _ => None
  }

  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case BinaryArgs(Proof(Not(sentence), IdentityRule, _, Only(majorAssumption), _), minorProof) => minorProof match {
      case Proof(`sentence`,_,_, pminor, _) =>
        Some(Proof(Absurdity, this, args, pminor + majorAssumption))
      case _ => None
    }
    case _ => None
  }
  
  override def toString = "~E"
}

case object AndElimination extends BaseRule {
  def major(sentence: Sentence) = sentence match {
    case And(_,_) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = true
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ And(left, right)) =>
      Some(
        BinaryParams(
          EmptyProof(And(left, right)),
          RelevantProof(goal, Variate(Assumption(left, bind), Assumption(right, bind)), Assumption(sentence))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case BinaryArgs(
      major@Proof(And(left, right), IdentityRule, _, Only(majorAssumption), _),
      minor@Proof(conc, _, _, assumptions,_))
      if (conc is (goal or Absurdity))
         and ((minor uses left) or (minor uses right)) =>
      Some(Proof(conc, this, args, assumptions.discharge(left, right) + majorAssumption, bind))
    case _ => None
  }
  
  override def toString = "&E"
}

case object OrElimination extends BaseRule {
  def major(sentence: Sentence) = sentence match {
    case Or(_,_) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = true
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ Or(left, right)) =>
      Some(TernaryParams(
        EmptyProof(Or(left, right)),
        RelevantProof(goal, Required(Assumption(left, bind)), Assumption(Or(left, right))),
        RelevantProof(goal, Required(Assumption(right, bind)), Assumption(Or(left, right)))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case TernaryArgs(
      major@Proof(Or(left, right), IdentityRule, _, Only(majorAssumption), _),
      minorLeft@Proof(concLeft, _, _, assumptionsLeft, _),
      minorRight@Proof(concRight, _, _, assumptionsRight, _))
      if ((concLeft is Absurdity) and (concRight is Absurdity))
         and ((minorLeft uses left) and (minorRight uses right)) =>
      Some(Proof(Absurdity, this, args, (assumptionsLeft ++ assumptionsRight).discharge(left, right) + majorAssumption, bind))
    case TernaryArgs(
      major@Proof(Or(left, right), IdentityRule, _, Only(majorAssumption), _),
      minorLeft@Proof(concLeft, _, _, assumptionsLeft, _),
      minorRight@Proof(concRight, _, _, assumptionsRight, _))
      if ((concLeft is goal) and (concRight is (Absurdity or goal)))
         and ((minorLeft uses left) and (minorRight uses right)) =>
      Some(Proof(concLeft, this, args, (assumptionsLeft ++ assumptionsRight).discharge(left, right) + majorAssumption, bind))
    case TernaryArgs(
      major@Proof(Or(left, right), IdentityRule, _, Only(majorAssumption), _),
      minorLeft@Proof(concLeft, _, _, assumptionsLeft, _),
      minorRight@Proof(concRight, _, _, assumptionsRight, _))
      if ((concRight is goal) and (concLeft is (Absurdity or goal)))
         and ((minorLeft uses left) and (minorRight uses right)) =>
      Some(Proof(concRight, this, args, (assumptionsLeft ++ assumptionsRight).discharge(left, right) + majorAssumption, bind))
    case _ => None
  }
  
  override def toString = "vE"
}

case object IfElimination extends BaseRule {
  def major(sentence: Sentence) = sentence match {
    case Implies(_,_) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = true
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ Implies(ante, conseq)) =>
      Some(TernaryParams(
        EmptyProof(sentence),
        RelevantProof(ante, Vacuous(), Assumption(sentence)),
        RelevantProof(goal, Required(Assumption(conseq, bind)), Assumption(sentence))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case TernaryArgs(
      major@Proof(Implies(ante, cons), IdentityRule,_, Only(majorAssumption), _),
      minorAnte,
      minorCons@Proof(conc, _, _, assumptionsCons, _))
        if (conc is (goal or Absurdity)) and (minorCons uses cons) =>
        minorAnte match {
          case Proof(`ante`,_,_, assumptionsAnte, _) =>
            Some(Proof(conc, this, args, (assumptionsAnte ++ assumptionsCons).discharge(cons) + majorAssumption, bind))
          case _ => None
        }
    case _ => None
  }
  
  override def toString = ">E"
}
