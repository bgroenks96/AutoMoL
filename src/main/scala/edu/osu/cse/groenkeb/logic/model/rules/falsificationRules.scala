package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.rules.IncompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.AnyProof
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.rules.AbstractRule
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryParams
import edu.osu.cse.groenkeb.logic.Sentence

final case class NegationFalsification() extends Rule {
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_,_,_), _) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = sentence match { case s if s.matches(Sentences.absurdity()) => true; case _ => false }
  
  def infer(conc: Sentence)(args: RuleArgs) = {
    val negation = Sentences.not(conc)
    args match {
      case BinaryArgs(CompleteProof(Conclusion(`conc`, _, _), pa),
                      CompleteProof(Conclusion(`negation`, _, _), pb)) =>
                        CompleteResult(CompleteProof(Sentences.absurdity(), this, args, pa ++ pb))
      case _ => IncompleteResult(BinaryParams(AnyProof(conc), AnyProof(negation)))
    }
  }
  
  override def toString = "<!>"
}

