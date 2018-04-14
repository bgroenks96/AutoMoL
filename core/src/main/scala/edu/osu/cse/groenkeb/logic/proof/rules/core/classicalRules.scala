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
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = {
    val options = context.atoms.toSeq.map(a => {
      BinaryParams(RelevantProof(goal, Required(Assumption(AtomicSentence(a), bind))),
                   RelevantProof(goal, Required(Assumption(Not(AtomicSentence(a)), bind))))
    })
    Some(OptionParams(options:_*))
  }
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = {
    val atoms: Seq[Sentence] = context.atoms.toSeq.map(a => AtomicSentence(a))
    val negAtoms: Seq[Sentence] = context.atoms.toSeq.map(a => Not(AtomicSentence(a)))
    args match {
      case BinaryArgs(
        p1@Proof(c1, _, _, assumptionsLeft, _),
        p2@Proof(c2, _, _, assumptionsRight, _))
        if c1 is (goal or Absurdity) and (c2 is (goal or Absurdity)) =>
          val a1 = atoms.find(a => assumptionsLeft.exists(as => as.matches(a)))
          val a2 = negAtoms.find(a => assumptionsRight.exists(as => as.matches(a)))
          (a1, a2) match {
            case (Some(s1), Some(s2)) =>
              Some(Proof(goal, this, args, (assumptionsLeft ++ assumptionsRight).discharge(s1, s2), bind))
            case _ => None
          }
      case _ => None
    }
  }
  
  override def toString = "rDil"
}
