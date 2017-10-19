package edu.osu.cse.groenkeb.logic.model.rules

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.Conclusion
import edu.osu.cse.groenkeb.logic.proof.NullProof
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.proof.ProudPremise
import edu.osu.cse.groenkeb.logic.proof.rules.IdentityRule
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.utils.Empty

case class ModelRule(val model: FirstOrderModel) extends Rule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(AtomicSentence(_), IdentityRule(), _), Empty()) => true
    case NullProof() => true
    case _ => false
  }
  
  def minor(proof: Proof) = false
  
  def yields(conc: Sentence) = conc match {
    case AtomicSentence(atom) => model.verify(conc)
    case Absurdity => true
    case _ => false
  }
  
  def infer(conc: Sentence)(args: RuleArgs) = conc match {
    case AtomicSentence(atom) => args match {
      case EmptyArgs() if model.verify(conc) => CompleteResult(CompleteProof(Conclusion(conc, this, args), Set()))
      //case EmptyArgs() if !model.verify(conc) => CompleteResult(CompleteProof(Conclusion(Absurdity(), this, args), Nil))
      case _ => NullResult()
    }
    case Absurdity => args match {
      case UnaryArgs(CompleteProof(c, Empty())) if !model.verify(c.sentence) =>
        CompleteResult(CompleteProof(Conclusion(Absurdity, this, args), Set(Assumption(c.sentence))))
      case _ => NullResult()
    }
    case _ => NullResult()
  }
  
  override def toString = "M"
}
