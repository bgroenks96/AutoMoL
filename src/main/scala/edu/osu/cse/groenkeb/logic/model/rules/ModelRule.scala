package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.Absurdity

case class ModelRule(val model: FirstOrderModel) extends Rule {
  def accepts(proof: Proof) = proof match {
    case NullProof(_) => true
    case _ => false
  }
  
  def yields(conc: Sentence) = conc match {
    case AtomicSentence(atom) => model.verify(conc)
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case AtomicSentence(atom) => args match {
      case EmptyArgs() if model.verify(conc) => CompleteResult(CompleteProof(Conclusion(conc, this, args), Nil))
      case EmptyArgs() if !model.verify(conc) => CompleteResult(CompleteProof(Conclusion(Absurdity(), this, args), Nil))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
}
