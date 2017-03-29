package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise

case class AndIntroductionRule() extends AbstractRule {
  def parity = Introduction(this)
  
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
      case UnaryArgs(CompleteProof(`a`, pa)) => IncompleteResult(UnaryParams(AnyProof(b)))
      case UnaryArgs(CompleteProof(`b`, pb)) => IncompleteResult(UnaryParams(AnyProof(a)))
      case _ => IncompleteResult(BinaryParams(AnyProof(a), AnyProof(b)))
    }
    case _ => NullResult()
  }
}
