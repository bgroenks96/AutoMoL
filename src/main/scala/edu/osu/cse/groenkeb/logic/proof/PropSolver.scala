package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.types._

class PropSolver(selector: PremiseSelector) {
  def prove(sentence: Sentence)(implicit context: ProofContext): ProofSearch = searchAndReduce(sentence, context.premises)(context.withGoal(sentence))

  private def searchPremises(conc: Sentence)(implicit context: ProofContext) = searchAndReduce(conc, selector.select(conc))

  private def args(params: RuleParams)(implicit context: ProofContext): RuleArgs = params match {
    case UnaryParams(param0) => UnaryArgs(prove(param0))
    case BinaryParams(param0, param1) => BinaryArgs(prove(param0), prove(param1))
    case _ => throw new Exception()
  }

  private def prove(param: RuleParam)(implicit context: ProofContext): Proof = param match {
    case AnyProof(conc) => searchPremises(conc)(context.withGoal(conc)).proof
    case RelevantProof(conc, discharge) => discharge match {
      case Required(prem) => prove(conc)(context.withAssumption(Assumption(prem))).proof match {
        case CompleteProof(conc, prems) if prems.exists(p => p.sentence.matches(prem)) => CompleteProof(conc, prems)
        case _ => NullProof(context.premises)
      }
      case Vacuous(prem) => prove(conc)(context.withAssumption(Assumption(prem))).proof
      case Composite(discharges) => null
      case Variate(discharges) => null
    }
  }

  private def tryInference(conc: Sentence, rule: Rule)(implicit context: ProofContext) = rule.infer(conc)(EmptyArgs()) match {
    // case for trivial (tautological) proof
    case CompleteResult(proof) => proof
    // case for decomposing introduction rule
    case IncompleteResult(params) => rule.infer(conc)(args(params)) match {
      case CompleteResult(proof) => proof
      case _ => NullProof(context.premises)
    }
    // case for non-applicable rules
    case NullResult() => NullProof(context.premises)
  }

  private def successFrom(proof: CompleteProof, continuation: Unit => ProofSearch)(implicit context: ProofContext) = {
    ProofSearch(proof.conc.sentence, Success(proof, continuation))
  }

  private def failureFrom(conc: Sentence, proof: NullProof)(implicit context: ProofContext) = {
    ProofSearch(conc, Failure(proof))
  }

  private def proveAndReduce(conc: Sentence, rules: Seq[Rule])(implicit context: ProofContext): ProofSearch = rules match {
    case Seq(rule, next@_*) => tryInference(conc, rule) match {
      case CompleteProof(c, p) => successFrom(CompleteProof(c, p), Unit => proveAndReduce(conc, next))
      case NullProof(p) => proveAndReduce(conc, next)
    }
    case Nil => failureFrom(conc, NullProof(context.premises))
  }

  private def searchAndReduce(conc: Sentence, prems: Seq[Premise])(implicit context: ProofContext): ProofSearch = {
    def composeResult(res: ProofSearch, next: Seq[Premise]) = res match {
      case ProofSearch(s, Success(p, fn)) => {
        val reduce = (u: Unit) => searchAndReduce(conc, next)
        ProofSearch(s, Success(p, reduce.compose { u => fn.apply(u) }))
      }
      case ProofSearch(s, Failure(p)) => searchAndReduce(conc, next)
    }

    prems match {
      case Seq(premise, next@_*) => premise match {
        case premise if premise.sentence.matches(conc) => successFrom(ProudPremise(premise.sentence).proof, Unit => searchAndReduce(conc, next))
        case ProudPremise(s) if conc.contains(s) => composeResult(proveAndReduce(conc, context.rules.accepting(ProudPremise(s).proof).yielding(conc)), next)
        case Assumption(s) if conc.contains(s) => composeResult(proveAndReduce(conc, context.rules.accepting(ProudPremise(s).proof).yielding(conc)), next)
        case _ => searchAndReduce(conc, next)
      }
      case Nil => ProofSearch(conc, Failure(NullProof(context.premises)))
    }
  }
}