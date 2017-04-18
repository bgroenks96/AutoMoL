package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.Or
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.rules.AnyProof
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyProof
import edu.osu.cse.groenkeb.logic.proof.rules.IncompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.OptionParams
import edu.osu.cse.groenkeb.logic.proof.rules.RelevantProof
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.Variate
import edu.osu.cse.groenkeb.logic.proof.types.Assumption
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.AbstractRule
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.Implies

abstract class FalsificationRule extends AbstractRule() {
  def yields(sentence: Sentence) = sentence match { case Absurdity() => true; case _ => false }
}

case class NegationFalsification() extends FalsificationRule {
  def accepts(proof: Proof) = proof match { case CompleteProof(_, _) => true; case _ => false }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity() => {
      val negation = Sentences.not(conc)
      args match {
        case BinaryArgs(CompleteProof(Conclusion(`conc`, _, _), pa),
                        CompleteProof(Conclusion(`negation`, _, _), pb)) =>
                          CompleteResult(CompleteProof(Absurdity(), this, args, pa ++ pb))
        case _ => IncompleteResult(BinaryParams(AnyProof(conc), AnyProof(negation)))
      }
    }
    case _ => NullResult()
  }
  
  override def toString = "<Not-F>"
}

case class AndFalsification() extends FalsificationRule {
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(_, _, And()), _, _), _) => true
    case CompleteProof(Conclusion(Absurdity(), _, _), _) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity() => args match {
      case BinaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, And()), _, _), Nil),
                      CompleteProof(Conclusion(Absurdity(), _, _), prems)) if prems exists { p => p.matches(left) || p.matches(right) } =>
                        CompleteResult(CompleteProof(Conclusion(Absurdity(), this, args), prems))
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, And()), _, _), Nil)) => 
        IncompleteResult(BinaryParams(EmptyProof(BinarySentence(left, right, And())),
                                      RelevantProof(Absurdity(), Variate(Assumption(left), Assumption(right)))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
  
  override def toString = "<And-F>"
}

case class OrFalsification() extends FalsificationRule() {
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(x, y, Or()), _, _), Nil) => true
    case CompleteProof(Conclusion(Absurdity(), _, _), _) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity() => args match {
      case TernaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, Or()), _, _), Nil), arg1, arg2) => (arg1, arg2) match {
        case (CompleteProof(Conclusion(Absurdity(), _, _), pleft), CompleteProof(Conclusion(Absurdity(), _, _), pright))
          if exists(left).in(pleft) && exists(right).in(pright) =>
            CompleteResult(CompleteProof(Conclusion(Absurdity(), this, args), pleft ++ pright))
      }
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(left, right, Or()), _, _), Nil)) =>
        IncompleteResult(TernaryParams(EmptyProof(BinarySentence(left, right, Or())),
                                       RelevantProof(Absurdity(), Required(Assumption(left))),
                                       RelevantProof(Absurdity(), Required(Assumption(right)))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
  
  override def toString = "<Or-F>"
}

case class ConditionalFalsification() extends FalsificationRule() {
  def accepts(proof: Proof) = proof match {
    case CompleteProof(_, _) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case Absurdity() => args match {
      case TernaryArgs(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), _, _), Nil), arg1, arg2) => (arg1, arg2) match {
        case (CompleteProof(Conclusion(`ante`, _, _), aprems), CompleteProof(Conclusion(Absurdity(), _, _), cprems))
          if exists(conseq).in(cprems) =>
            CompleteResult(CompleteProof(Conclusion(Absurdity(), this, args), aprems ++ cprems))
      }
      case UnaryArgs(CompleteProof(Conclusion(BinarySentence(ante, conseq, Implies()), _, _), Nil)) =>
        IncompleteResult(TernaryParams(EmptyProof(BinarySentence(ante, conseq, Implies())),
                                       AnyProof(ante),
                                       RelevantProof(Absurdity(), Required(Assumption(conseq)))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
  
  override def toString = "<Cond-F>"
}

