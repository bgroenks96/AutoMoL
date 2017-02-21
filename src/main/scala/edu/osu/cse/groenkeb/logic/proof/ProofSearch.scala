package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.types._

object ProofSearch {
  def findProof(sentence: Sentence)(implicit context: ProofContext): Seq[Proof] = {
    findProof(sentence.toRelation)
  }

  protected def findProof(conclusion: ObjectRelation)(implicit context: ProofContext): Seq[Proof] = conclusion match {
    case SentenceRelation(AtomicSentence(a)) => null //toRelations(context.premises).filter(r => r.contains(conclusion))
    case relation => context.rules.yielding(relation) match {
      case RuleSet() => List(NullProof(context.premises))
      case RuleSet(rules @ _*) => rules.map { r => tryTrivialInference(relation, r) }.filter(p => p match {
        case CompleteProof(_, _) => true
        case NullProof(_) => false
      })
    }
  }

  protected def tryTrivialInference(conclusion: ObjectRelation, rule: Rule)(implicit context: ProofContext) = rule.infer(conclusion)() match {
    // case for trivial proof
    case CompleteResult(x) => CompleteProof(Conclusion(conclusion.sentenceResult, rule, NullPremise()), List())
    case IncompleteResult(reqs @ _*) => {
      val proofs = reqs.map(req => req match {
        case Require(SentenceRelation(x)) => findProof(x).find(isComplete)
      })
      if (proofs.forall(opt => opt.isEmpty)) {
        NullProof(context.premises)
      } else {
        CompleteProof(Conclusion(conclusion.sentenceResult, rule, proofs.head.get.conclusion.get, proofs.drop(1).map(opt => opt.get.conclusion.get):_*), context.premises)
      }
    }
    case NullResult() => NullProof(context.premises)
  }

  private def toRelations(premises: Seq[Premise]) = premises.map { p => p.sentence.toRelation }
  
  private def isComplete(proof: Proof) = proof match {
    case CompleteProof(_,_) => true
    case NullProof(_) => false
  }
}