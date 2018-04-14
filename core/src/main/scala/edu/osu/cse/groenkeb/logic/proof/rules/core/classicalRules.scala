package edu.osu.cse.groenkeb.logic.proof.rules.core

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.utils.Only

case object RestrictedDilemma extends BaseRule {
  def major(sentence: Sentence) = sentence match {
    case If(_,_) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = true
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = major match {
    case Some(sentence @ If(ante, conseq)) =>
      Some(TernaryParams(
        EmptyProof(sentence),
        RelevantProof(ante, Vacuous(), Assumption(sentence)),
        RelevantProof(goal, Required(Assumption(conseq, bind)), Assumption(sentence))))
    case _ => None
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case TernaryArgs(
      major@Proof(If(ante, cons), IdentityRule,_, Only(majorAssumption), _),
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
  
  override def toString = "rDil"
}
