package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.AbstractRule
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs
import edu.osu.cse.groenkeb.logic.Not
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.RelevantProof
import edu.osu.cse.groenkeb.logic.proof.rules.IncompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.types.Assumption

case class NegationVerification() extends AbstractRule() {  
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(`absurdity`, _, _), _) => true
    case _ => false
  }
  
  def yields(conc: Sentence) = conc match {
    case UnarySentence(_, Not()) => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case UnarySentence(sentence, Not()) => args match {
      case UnaryArgs(CompleteProof(Conclusion(`absurdity`, _, _), prems)) if  prems exists { p => p.matches(sentence) } =>
        CompleteResult(CompleteProof(Conclusion(sentence, this, args), prems))
      case _ => IncompleteResult(UnaryParams(RelevantProof(absurdity, Required(Assumption(sentence)))))
    }
    case _ => NullResult()
  }
}

case class AndVerification() extends AbstractRule() {
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false
  }
}
