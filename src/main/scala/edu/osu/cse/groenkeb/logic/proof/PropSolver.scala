package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.types._

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
        case EmptyArgs() => CompleteProof(s, rule, args, Nil)
        case UnaryArgs(proof0) => CompleteProof(s, rule, args, proof0.premises)
        case BinaryArgs(proof0, proof1) => CompleteProof(s, rule, args, proof0.premises ++ proof1.premises)
        case TernaryArgs(proof0, proof1, proof2) => CompleteProof(s, rule, args, proof0.premises ++ proof1.premises ++ proof2.premises)
      }
      case premise if premise.sentence.contains(context.goal) =>
        // if the premise contains our goal, attempt to infer conclusion from a rule that accepts the sentence as a major premise
        val proudProof = ProudPremise(premise.sentence).proof
        val relevantRules = strategy.rules(context.withRuleSet(context.rules.accepting(proudProof)))
        proofFromInference(relevantRules, UnaryArgs(proudProof)) match {
          case NullProof(_) => proofFromPremises(rem)
          case proof => proof
        }
      case _ => proofFromPremises(rem)
    }
  }
  
  private def proofByDecomposition(premise: Premise)(implicit context: ProofContext): Proof = premise.sentence.decompose() match {
    case Seq(unary) => then(proof(context.withGoal(unary)))
    case Seq(left, right) if right.contains(context.goal) => then(proof(context.withGoal(right)))
    case Seq(left, right) if left.contains(context.goal) => then(proof(context.withGoal(left)))
  }
  
  private def proofFromInference(rules: RuleSet, args: RuleArgs = EmptyArgs())(implicit context: ProofContext): Proof = rules match {
    case RuleSet(Nil) => NullProof(context.premises)
    case RuleSet(Seq(head, rem@_*)) => infer(head, args) match {
      case Left(NullProof(prems)) => proofFromInference(RuleSet(rem), args)
      case Left(proof) => proof
      case Right(params) => infer(head, proof(params)) match {
        case Left(CompleteProof(c, p)) => CompleteProof(c, p)
        case Left(NullProof(_)) | Right(_) => proofFromInference(RuleSet(rem), args)
      }
    }
  }
  
  private def relevantProof(discharge: Discharge, restrict: Assumption*)(implicit context: ProofContext): Proof = {
      val newContext = context.lessAssumptions(restrict:_*)
      discharge match {
        case Vacuous(assumptions@_*) => then(proof(newContext.withAssumptions(assumptions:_*)))
        case Required(assumptions@_*) => then(proof(newContext.withAssumptions(assumptions:_*))) match {
          case CompleteProof(c, p) if assumptions forall { a => p.contains(a.sentence) } => CompleteProof(c, p)
          case _ => NullProof(newContext.premises)
        }
        case Variate(assumptions@_*) => then(proof(newContext.withAssumptions(assumptions:_*))) match {
          case CompleteProof(c, prems) if prems exists { p => assumptions exists { a => a.sentence.matches(p.sentence) } } => CompleteProof(c, prems)
          case _ => NullProof(newContext.premises)
        }
      }
  }

  private def proof(param: RuleParam)(implicit context: ProofContext): Proof = param match {
    case EmptyProof(conc) => ProudPremise(conc).proof
    case AnyProof(conc) => then(proof(context.withGoal(conc)))
    case RelevantProof(conc, discharge, restrict) => relevantProof(discharge, restrict)(context.withGoal(conc))
    case _ => NullProof(context.premises)
  }

  private def proof(params: RuleParams)(implicit context: ProofContext): RuleArgs = params match {
    case UnaryParams(p0) => UnaryArgs(proof(p0))
    case BinaryParams(p0, p1) => BinaryArgs(proof(p0), proof(p1))
    case TernaryParams(p0, p1, p2) => TernaryArgs(proof(p0), proof(p1), proof(p2))
    case EmptyParams() => EmptyArgs()
  }

  private def infer(rule: Rule, args: RuleArgs = EmptyArgs())(implicit context: ProofContext): Either[Proof, RuleParams] = {
    rule.infer(context.goal)(args) match {
      // case for trivial (tautological) proof
      case CompleteResult(proof) => Left(proof)
      // case for decomposing introduction rule
      case IncompleteResult(params) => Right(params)
      // case for non-applicable rules
      case NullResult() => Left(NullProof(context.premises))
    }
  }
}