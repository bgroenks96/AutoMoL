package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.Proof

abstract class AbstractRule extends Rule {
  def exists(sentences: Sentence*) = CaseAssumptions(sentences:_*)
}

final case class IdentityRule() extends AbstractRule {
  def major(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false
  }

  def minor(proof: Proof) = false

  def yields(sentence: Sentence) = true

  def infer(conc: Sentence)(args: RuleArgs) = args match {
    case UnaryArgs(CompleteProof(Conclusion(`conc`, _, _), prems)) =>
      CompleteResult(CompleteProof(`conc`, this, args, prems))
    case _ => IncompleteResult(UnaryParams(AnyProof(conc)))
  }

  override def toString = "<id>"
}

final case class NullRule() extends AbstractRule {
  def major(proof: Proof) = false

  def minor(proof: Proof) = false

  def yields(sentence: Sentence) = false

  def infer(conc: Sentence)(args: RuleArgs) = NullResult()

  override def toString = "<>"
}

protected case class CaseAssumptions(sentences: Sentence*) {
  def in[T <: Premise](prems: Traversable[T]) = prems.forall { p => sentences.forall { s => p.matches(s) } }
}
