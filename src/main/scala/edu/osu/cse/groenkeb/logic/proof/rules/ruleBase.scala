package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.Proof

abstract class AbstractRule extends Rule {
  def hasPremise(premises: Seq[Premise], sentence: Sentence) = {
    premises.exists(p => sentence.matches(p.sentence))
  }
}

final case class IdentityRule protected() extends AbstractRule {
  def parity = None(this)

  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false
  }

  def yields(sentence: Sentence) = true

  def infer(conc: Sentence)(args: RuleArgs) = args match {
    case UnaryArgs(CompleteProof(Conclusion(`conc`, _, _), prems)) =>
      CompleteResult(CompleteProof(`conc`, this, args, prems))
    case _ => IncompleteResult(UnaryParams(AnyProof(conc)))
  }
}

final case class NonContradictionRule protected() extends AbstractRule {
  def parity = None(this)
  
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
}

final case class NullRule protected() extends AbstractRule {
  def parity = None(this)

  def accepts(proof: Proof) = false

  def yields(sentence: Sentence) = false

  def infer(conc: Sentence)(args: RuleArgs) = NullResult()
}

