package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.proof.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.Conclusion
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.utils.Empty

@Deprecated
case class AndIntroductionRule() extends AbstractRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(s, _, _), _) if s != Absurdity => true
    case _ => false
  }
  
  // and-intro criteria for minor premise identical to major
  def minor(proof: Proof) = major(proof)

  def yields(sentence: Sentence) = sentence match {
    case BinarySentence(_, _, And()) => true
    case _ => false
  }

  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case BinarySentence(a, b, And()) => args match {
      case BinaryArgs(CompleteProof(Conclusion(`a`, _, _), pa),
        CompleteProof(Conclusion(`b`, _, _), pb)) =>
        CompleteResult(CompleteProof(conc, this, args, pa ++ pb))
      //case UnaryArgs(CompleteProof(`a`, pa)) => IncompleteResult(UnaryParams(AnyProof(b)))
      //case UnaryArgs(CompleteProof(`b`, pb)) => IncompleteResult(UnaryParams(AnyProof(a)))
      case _ => IncompleteResult(BinaryParams(AnyProof(a), AnyProof(b)))
    }
    case _ => NullResult()
  }

  override def toString = "<&-Intro>"
}

@Deprecated
case class AndEliminationRule() extends AbstractRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(a, b, And()), _, _), Empty()) => true
    case _ => false
  }
  
  def minor(proof: Proof) = proof match {
    case CompleteProof(Conclusion(s,_,_), _) if s != Absurdity => true
    case _ => false
  }

  def yields(sentence: Sentence) = true

  def infer(conc: Sentence)(args: RuleArgs) = args match {
    case BinaryArgs(CompleteProof(Conclusion(BinarySentence(a, b, And()), _, _), Empty()), proof) => proof match {
      case CompleteProof(Conclusion(`conc`, _, _), prems) if prems.exists(p => p.sentence.matches(a) || p.sentence.matches(b)) =>
        CompleteResult(CompleteProof(conc, this, args, prems))
      case _ => NullResult()
    }
    case UnaryArgs(CompleteProof(Conclusion(BinarySentence(a, b, And()), _, _), Empty())) =>
      IncompleteResult(BinaryParams(EmptyProof(BinarySentence(a, b, And())),
        RelevantProof(conc, Variate(Assumption(a), Assumption(b)), Assumption(BinarySentence(a, b, And())))))
    case _ => NullResult()
  }

  override def toString = "<&-Elim>"
}
