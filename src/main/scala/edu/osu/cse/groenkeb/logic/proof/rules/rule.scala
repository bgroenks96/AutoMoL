package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.SentenceRelation
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise

trait Rule {
  /**
   * The "parity" of this rule; must be either elimination, introduction, or none.
   */
  def parity: RuleParity

  /**
   * True if this rule accepts the given proof as a premise, false otherwise.
   */
  def accepts(proof: Proof): Boolean

  /**
   * True if this rule yields the given object type as a conclusion, false otherwise.
   */
  def yields(obj: ObjectRelation): Boolean

  /**
   * Try to infer a given object relation from the given set of proven premises.
   */
  def infer(conclusion: ObjectRelation)(from: RuleArgs): InferenceResult
}

abstract class BaseRule extends Rule {
  def hasPremise(premises: Seq[Premise], sentence: Sentence) = {
    premises.exists(p => sentence.matches(p.sentence))
  }
}

case class ReflexivityRule() extends BaseRule {
  def parity = None(this)

  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false
  }

  def yields(obj: ObjectRelation) = true

  def infer(conc: ObjectRelation)(args: RuleArgs) = args match {
    case UnaryArgs(CompleteProof(Conclusion(conc, _, _), _)) =>
      CompleteResult(CompleteProof(conc, this, args, ProudPremise(conc.toSentence) :: Nil))
    case _ => IncompleteResult(UnaryParams(AnyProof(conc)))
  }
}

case class AndIntroductionRule() extends BaseRule {
  def parity = Introduction(this)
  
  def accepts(proof: Proof) = proof match {
    case CompleteProof(Conclusion(_, _, _), _) => true
    case _ => false    
  }
  
  def yields(obj: ObjectRelation) = obj match {
    case And(_,_) => true
    case _ => false
  }
  
  def infer(conc: ObjectRelation)(args: RuleArgs) = conc match {
    case And(a, b) => args match {
      case BinaryArgs(CompleteProof(cleft, _),
                      CompleteProof(cright, _)) =>
        CompleteResult(CompleteProof(conc, this, args, cleft :: cright :: Nil))
      case _ => IncompleteResult(BinaryParams(AnyProof(a.toRelation), AnyProof(b.toRelation)))
    }
    case _ => NullResult()
  }
}

case class NullRule() extends BaseRule {
  def parity = None(this)

  def accepts(proof: Proof) = false

  def yields(obj: ObjectRelation) = false

  def infer(conc: ObjectRelation)(args: RuleArgs) = NullResult()
}
