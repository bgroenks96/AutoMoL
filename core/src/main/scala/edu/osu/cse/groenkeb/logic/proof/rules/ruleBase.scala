package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.Conclusion
import edu.osu.cse.groenkeb.logic.proof.Premise
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.proof.Conclusion
import edu.osu.cse.groenkeb.logic.proof.Premise
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.Not
import edu.osu.cse.groenkeb.logic.UnarySentence
import edu.osu.cse.groenkeb.logic.Absurdity

abstract class BaseRule extends Rule {
  def exists(sentences: Sentence*) = CaseAssumptions(sentences:_*)
}

final case class IdentityRule() extends BaseRule {
  def yields(sentence: Sentence) = true

  def infer(conc: Sentence)(args: RuleArgs) = args match {
    case UnaryArgs(Proof(Conclusion(`conc`, _, _), prems)) =>
      CompleteResult(Proof(`conc`, this, args, prems + Assumption(conc)))
    case _ => IncompleteResult(UnaryParams(AnyProof(conc)))
  }

  override def toString = "id"
}

final case class NullRule() extends BaseRule {
  def major(proof: Proof) = false

  def yields(sentence: Sentence) = false

  def infer(conc: Sentence)(args: RuleArgs) = NullResult()

  override def toString = "nil"
}

final case class NonContradictionRule() extends BaseRule {
  def minor(proof: Proof) = proof match {
    case Proof(Conclusion(UnarySentence(_, Not()),_,_), _) => true
    case _ => false
  }
  
  def yields(sentence: Sentence) = sentence match { case Absurdity => true; case _ => false }
  
  def infer(conc: Sentence)(args: RuleArgs) = {
    val negation = Sentences.not(conc)
    args match {
      case BinaryArgs(Proof(Conclusion(`conc`, _, _), pa),
                      Proof(Conclusion(`negation`, _, _), pb)) =>
                        CompleteResult(Proof(Absurdity, this, args, pa ++ pb))
      case _ => IncompleteResult(BinaryParams(AnyProof(conc), AnyProof(negation)))
    }
  }
  
  override def toString = "<NonContradiction>"
}

protected case class CaseAssumptions(sentences: Sentence*) {
  def in(prems: Traversable[Premise]) = sentences forall { s => prems exists { p => p.matches(s) }}
}
