package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.types._

class ProofSearch(context: ProofContext) {
  def findProof(sentence: Sentence): Seq[Proof] = {
    findProof(sentence.toRelation, context.premises)
  }

  protected def findProof(relation: ObjectRelation, premises: Seq[Premise]): Seq[Proof] = {
    context.rules.yielding(relation) match {
      case RuleSet() => List(NullProof(premises))
      case RuleSet(rules @ _*) => rules.map { r => tryInference(relation, r, premises) }.filter(p => p match {
        case CompleteProof(_, _) => true
        case NullProof(_) => false
      })
    }
  }

  protected def tryInference(conclusion: ObjectRelation, rule: Rule, premises: Seq[Premise]) = rule.infer(conclusion)() match {
    // case for trivial proof
    case CompleteResult(x) => CompleteProof(Conclusion(conclusion.sentenceResult, rule, NullPremise()), List())
    case IncompleteResult(req @ _*) => CompleteProof(Conclusion(conclusion.sentenceResult,
                                                                rule,
                                                                ProudPremise(req.head.sentenceResult),
                                                                req.drop(1).map(r => provePremise(r, premises)).filter(x => x.nonEmpty).map(c => c.get) : _*),
                                                     premises)
    case NullResult() => NullProof(premises)
  }

  protected def provePremise(premise: ObjectRelation, given: Seq[Premise]) = findProof(premise, given).head.conclusion

  private def toRelations(premises: Seq[Premise]) = premises.map { p => p.sentence.toRelation }
}