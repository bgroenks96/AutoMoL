package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.Implies
import edu.osu.cse.groenkeb.logic.Not
import edu.osu.cse.groenkeb.logic.Or
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.proof.rules.AbstractRule
import edu.osu.cse.groenkeb.logic.proof.rules.AnyProof
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyProof
import edu.osu.cse.groenkeb.logic.proof.rules.IncompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.RelevantProof
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.Variate
import edu.osu.cse.groenkeb.logic.proof.types.Assumption
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.utils.Empty

abstract class FalsificationRule extends AbstractRule() {
  def yields(sentence: Sentence) = sentence match { case Absurdity => true; case _ => false }
}

case class NegationFalsification() extends FalsificationRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(UnarySentence(_, Not()),_,_), _) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case BinaryArgs(CompleteProof(Conclusion(UnarySentence(operand, Not()), _, _), pa), minorProof) => {
        minorProof match {
          case CompleteProof(Conclusion(`operand`, _, _), pb) => 
            val major = Assumption(UnarySentence(operand, Not()))
            CompleteResult(CompleteProof(Absurdity, this, args, pa ++ pb + major))
          case _ => IncompleteResult(BinaryParams(AnyProof(Sentences.not(operand)), AnyProof(operand)))
        }
      }
      case UnaryArgs(CompleteProof(Conclusion(UnarySentence(s, Not()), _, _), pa)) => IncompleteResult(BinaryParams(EmptyProof(Sentences.not(s)), AnyProof(s)))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = "~F"
}

case class AndFalsification() extends FalsificationRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(_, _, And()), _, _), _) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case BinaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, And()), _, _), Empty()),
                      CompleteProof(Conclusion(Absurdity, _, _), prems)) if prems exists { p => p.matches(left) || p.matches(right) } =>
                        val major = Assumption(BinarySentence(left, right, And()))
                        val discharges = Set(Assumption(left), Assumption(right))
                        CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), prems -- discharges + major))
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, And()), _, _), Empty())) =>
        IncompleteResult(BinaryParams(EmptyProof(BinarySentence(left, right, And())),
                                      RelevantProof(Absurdity, Variate(Assumption(left), Assumption(right)), Assumption(BinarySentence(left, right, And())))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = "&F"
}

case class OrFalsification() extends FalsificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(x, y, Or()), _, _), Empty()) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case TernaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, Or()), _, _), Empty()), arg1, arg2) => (arg1, arg2) match {
        case (CompleteProof(Conclusion(Absurdity, _, _), pleft), CompleteProof(Conclusion(Absurdity, _, _), pright))
          if exists(left).in(pleft) && exists(right).in(pright) =>
            val major = Assumption(BinarySentence(left, right, Or()))
            val discharges = Set(Assumption(left), Assumption(right))
            CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), pleft ++ pright -- discharges + major))
        case _ => NullResult()
      }
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, Or()), _, _), Empty())) =>
        IncompleteResult(TernaryParams(EmptyProof(BinarySentence(left, right, Or())),
                                       RelevantProof(Absurdity, Required(Assumption(left)), Assumption(BinarySentence(left, right, Or()))),
                                       RelevantProof(Absurdity, Required(Assumption(right)), Assumption(BinarySentence(left, right, Or())))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = "+F"
}

case class ConditionalFalsification() extends FalsificationRule() {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(_,_, Implies()),_,_), _) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity => args match {
      case TernaryArgs(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), _, _), Empty()), arg1, arg2) => (arg1, arg2) match {
        case (CompleteProof(Conclusion(`ante`, _, _), aprems), CompleteProof(Conclusion(Absurdity, _, _), cprems))
          if exists(conseq).in(cprems) =>
            val major = Assumption(BinarySentence(ante, conseq, Implies()))
            val discharges = Set(Assumption(conseq))
            CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), aprems ++ cprems -- discharges + major))
      }
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), _, _), Empty())) =>
        IncompleteResult(TernaryParams(EmptyProof(BinarySentence(ante, conseq, Implies())),
                                       AnyProof(ante),
                                       RelevantProof(Absurdity, Required(Assumption(conseq)), Assumption(BinarySentence(ante, conseq, Implies())))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }

  override def toString = ">F"
}
