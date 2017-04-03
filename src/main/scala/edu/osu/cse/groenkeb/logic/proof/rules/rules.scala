package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.Assumption

case class AndIntroductionRule() extends AbstractRule {
  def parity = Introduction(AndEliminationRule())

  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false
  }

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

case class AndEliminationRule() extends AbstractRule {
  def parity = Elimination(AndIntroductionRule())

  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(BinarySentence(a, b, And()), _, _), Nil) => true
    case _ => false
  }

  def yields(sentence: Sentence) = true

  def infer(conc: Sentence)(args: RuleArgs) = args match {
    case BinaryArgs(CompleteProof(Conclusion(BinarySentence(a, b, And()), _, _), Nil), proof) => proof match {
      case CompleteProof(Conclusion(`conc`, _, _), prems) if prems.exists(p => p.sentence.matches(a) || p.sentence.matches(b)) =>
        CompleteResult(CompleteProof(conc, this, args, prems))
      case _ => NullResult()
    }
    case UnaryArgs(CompleteProof(Conclusion(BinarySentence(a, b, And()), _, _), Nil)) =>
      IncompleteResult(BinaryParams(EmptyProof(BinarySentence(a, b, And())),
        RelevantProof(conc, Variate(Assumption(a), Assumption(b)), Assumption(BinarySentence(a, b, And())))))
    case _ => NullResult()
  }

  override def toString = "<&-Elim>"
}
