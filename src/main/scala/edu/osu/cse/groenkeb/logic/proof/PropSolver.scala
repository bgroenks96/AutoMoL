package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.types._
import scala.collection.parallel.ParSeq

case class PropSolver(implicit strategy: ProofStrategy) extends Solver {
  def proof(implicit context: ProofContext): ProofResult = strategy.decide(proofFromPremises(strategy.premises))
  
  private def then(result: ProofResult, continue: () => Proof = null): Proof = result match {
    case Failure(proof, Continue()) if continue != null => continue()
    case finish => finish.proof
  }
  
  private def proofFromPremises(premises: Seq[Premise])(implicit context: ProofContext): Proof = premises match {
    case Nil => proofFromInference(RuleSet(strategy.rules))
    case Seq(head, rem@_*) => head match {
      case ProudPremise(s) if s.matches(context.goal) => ProudPremise(s).proof
      case Assumption(s) if s.matches(context.goal) => Assumption(s).proof
      case Conclusion(s, rule, args) if s.matches(context.goal) => args match {
        case UnaryArgs(proof0) => CompleteProof(s, rule, args, proof0.premises)
        case BinaryArgs(proof0, proof1) => CompleteProof(s, rule, args, proof0.premises ++ proof1.premises)
        case TernaryArgs(proof0, proof1, proof2) => CompleteProof(s, rule, args, proof0.premises ++ proof1.premises ++ proof2.premises)
      }
      // if the premise contains our goal, attempt to infer conclusion from a rule that accepts the sentence as a major premise
      case premise if premise.sentence.contains(context.goal) => proofFromInference(strategy.rules(context.withRuleSet(context.rules.accepting(ProudPremise(premise.sentence).proof))))
      case _ => proofFromPremises(rem)
    }
  }
  
  private def proofByDecomposition(premise: Premise)(implicit context: ProofContext): Proof = premise.sentence.decompose() match {
    case Seq(unary) => then(proof(context.withGoal(unary)))
    case Seq(left, right) if right.contains(context.goal) => then(proof(context.withGoal(right)))
    case Seq(left, right) if left.contains(context.goal) => then(proof(context.withGoal(left)))
  }
  
  private def proofFromInference(rules: RuleSet)(implicit context: ProofContext): Proof = rules match {
    case RuleSet(Nil) => NullProof(context.premises)
    case RuleSet(Seq(head, rem@_*)) => infer(head) match {
      case Left(NullProof(prems)) => proofFromInference(RuleSet(rem))
      case Left(proof) => proof
      case Right(params) => infer(head, proof(params)) match {
        case NullProof(prems) => proofFromInference(RuleSet(rem))
        case proof => proof
      }
    }
  }
  
  private def relevantProof(conc: Sentence, discharge: Discharge, restrict: Seq[Assumption])(implicit context: ProofContext): Proof = {
      val newContext = context.withGoal(conc).lessAssumptions(restrict:_*)
      discharge match {
        case Vacuous(assumption) => then(proof(newContext.withAssumption(assumption)))
        case Required(assumption) => then(proof(newContext.withAssumption(assumption))) match {
          case CompleteProof(c, p) if p.contains(assumption.sentence) => CompleteProof(c, p)
          case _ => NullProof(context.premises)
        }
        case Variate(discharges@_*) => null
        case Composite(discharges@_*) => null
      }
  }

  private def proof(param: RuleParam)(implicit context: ProofContext): Proof = param match {
    case AnyProof(conc) => then(proof(context.withGoal(conc)))
    case RelevantProof(conc, discharge, restrict) => relevantProof(conc, discharge, restrict)
  }

  private def proof(params: RuleParams)(implicit context: ProofContext): RuleArgs = params match {
    case UnaryParams(p0) => UnaryArgs(proof(p0))
    case BinaryParams(p0, p1) => BinaryArgs(proof(p0), proof(p1))
    case TernaryParams(p0, p1, p2) => TernaryArgs(proof(p0), proof(p1), proof(p2))
  }

  private def infer(rule: Rule, args: RuleArgs)(implicit context: ProofContext): Proof = {
    rule.infer(context.goal)(args) match {
      case CompleteResult(proof) => proof
      case _ => NullProof(context.premises)
    }
  }

  private def infer(rule: Rule)(implicit context: ProofContext): Either[Proof, RuleParams] = {
    rule.infer(context.goal)(EmptyArgs()) match {
      // case for trivial (tautological) proof
      case CompleteResult(proof) => Left(proof)
      // case for decomposing introduction rule
      case IncompleteResult(params) => Right(params)
      // case for non-applicable rules
      case NullResult() => Left(NullProof(context.premises))
    }
  }
}