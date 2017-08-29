package edu.osu.cse.groenkeb.logic.proof

import scala.Left
import scala.Right
import scala.collection.immutable.Nil
import scala.collection.immutable.Seq
import scala.collection.immutable.Set
import scala.collection.immutable.Stream

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.rules.AnyProof
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.BinaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.CompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyParams
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyProof
import edu.osu.cse.groenkeb.logic.proof.rules.IncompleteResult
import edu.osu.cse.groenkeb.logic.proof.rules.NullResult
import edu.osu.cse.groenkeb.logic.proof.rules.OptionArgs
import edu.osu.cse.groenkeb.logic.proof.rules.OptionParams
import edu.osu.cse.groenkeb.logic.proof.rules.RelevantProof
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.RuleParam
import edu.osu.cse.groenkeb.logic.proof.rules.RuleParams
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.TernaryParams
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryParams
import edu.osu.cse.groenkeb.logic.proof.types.Assumption
import edu.osu.cse.groenkeb.logic.proof.types.CompleteProof
import edu.osu.cse.groenkeb.logic.proof.types.Conclusion
import edu.osu.cse.groenkeb.logic.proof.types.NullProof
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise

class ProofSolver(strategy: ProofStrategy = new NaiveProofStrategy()) {
  
  def prove(context: ProofContext): Stream[ProofResult] = {
    implicit val cntxt = context
    ProofSearch(step { proof(strategy.premises) } )()
  }
  
  private def proof(premises: scala.Seq[Premise])(implicit context: ProofContext): ProofResult = context.goal match {
    case Absurdity() => premises match {
      case Nil => failure()
      case Seq(head, rem@_*) => head match {
        case s => {
          var relevantRules = strategy.rules.acceptingMajor(ProudPremise(s.sentence).proof).yielding(Absurdity())
                  inferFrom(relevantRules, UnaryArgs(ProudPremise(s.sentence).proof)) match {
                    case Success(cp, cntxt, prems) => success(cp, { proof(rem) })
                    case Failure(cntxt, hint) => failure(hint -> Continue(step({ proof(rem) })))
                    case pending:Pending => pending
          }}
      }
    }
    case goal => premises match {
      case Nil => inferFrom(RuleSet(strategy.rules))
      case Seq(head, rem@_*) => head match {
        case ProudPremise(s) if context.hasGoal(s) => success(ProudPremise(s).proof, proof(rem))
        case Assumption(s) if context.hasGoal(s) => success(ProudPremise(s).proof, proof(rem))
        case Conclusion(s, rule, args) if context.hasGoal(s) => args match {
          case EmptyArgs() => success(CompleteProof(s, rule, args, Set()), proof(rem))
          case UnaryArgs(proof0) => success(CompleteProof(s, rule, args, proof0.premises), proof(rem))
          case BinaryArgs(proof0, proof1) => 
            success(CompleteProof(s, rule, args, proof0.premises ++ proof1.premises), proof(rem))
          case TernaryArgs(proof0, proof1, proof2) =>
            success(CompleteProof(s, rule, args, proof0.premises ++ proof1.premises ++ proof2.premises), proof(rem))
          case OptionArgs(proofs@_*) =>
            success(CompleteProof(s, rule, args, proofs flatMap { p => p.premises } toSet), proof(rem))
        }
        case _ => proof(rem)
      }
    }
  }
  
  // NOT DONE YET!
  private def inferFrom(rules: RuleSet, args: RuleArgs = EmptyArgs())(implicit context: ProofContext): ProofResult = {
    // inferFromResults matches the given sequence of Success results to a RuleArgs type and performs the final inference
    // step. It is assumed that the given result has already been validated to fulfill the necessary inference requirements.
    // Failure to meet this precondition is an error on the part of the caller.
    def inferFromResults(completed: Success*)(implicit rule: Rule): CompleteProof = immutable(completed) match {
      case Seq(res0) => infer(rule, UnaryArgs(res0.proof)) match {
        case Left(proof:CompleteProof) => proof
        // Exceptional case: one or more of the supplied Success results failed to satisfy the precondition.
        // This would indicate a bug in the calling code.
        case r => throw new IllegalArgumentException("one or more arguments failed to satisfy the proof: " + r)
      }
      case Seq(res0, res1) => infer(rule, BinaryArgs(res0.proof, res1.proof)) match {
        case Left(proof:CompleteProof) => proof
        case r => throw new IllegalArgumentException("one or more arguments failed to satisfy the proof: " + r)
      }
      case Seq(res0, res1, res2) => infer(rule, TernaryArgs(res0.proof, res1.proof, res2.proof)) match {
        case Left(proof:CompleteProof) => proof
        case r => throw new IllegalArgumentException("one or more arguments failed to satisfy the proof: " + r)
      }
      // TODO handle multi-arg case
    }
    
    // filterSuccess filters the given proof results to include only those that are both successful and yield the given conclusion.
    def filterSuccess(results: Stream[ProofResult], conc: Sentence): Stream[Success] = results collect { 
      case r:Success if r.proof.conc.matches(conc) || r.proof.conc.conclusion.isAbsurdity => r.asInstanceOf[Success]
    }
    
    // advance discards the head of the first Stream in 'paramResults' that has more than a single result, leaving the remainder
    // of the Streams unchanged.
    def advance(paramResults: scala.collection.Seq[Stream[ProofResult]]): Seq[Stream[ProofResult]] = paramResults match {
      case Nil => Nil
      case Seq(Stream(head, next, tail@_*), rem@_*) => Seq(Stream.cons(next, { tail.toStream })) ++ rem
      case Seq(Stream(head), rem@_*) => Seq(Stream(head)) ++ advance(rem)
    }
    
    // ------ Aggregator Functions ------- //
    
    def unaryResults(p0: RuleParam)(resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]])(implicit rule: Rule): ProofResult =
      paramResults match {
        case Seq(results0) => filterSuccess(results0, p0.goal) match {
          case Stream() => failure()
          case Stream(head, rem@_*) => success(inferFromResults(head), { unaryResults(p0)(resultContext, Seq(rem.toStream)) })
          case Stream(head) => success(inferFromResults(head))
        }
        // unaryResults only supports a single set of results for a single parameter
        case _ => throw new IllegalArgumentException("unexpected parameter count for unary results")
      }
    
    
    def binaryResults(p0: RuleParam, p1: RuleParam)(resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]])
                     (implicit rule: Rule): ProofResult = {
      paramResults match {
        case Seq(results0, results1) => (filterSuccess(results0, p0.goal), filterSuccess(results1, p1.goal)) match {
          case (filtered0, filtered1) if filtered0.isEmpty || filtered1.isEmpty => failure()
          case (Stream(head0), Stream(head1)) => success(inferFromResults(head0, head1))
          case (filtered0, filtered1) => 
            success(inferFromResults(filtered0.head, filtered1.head), { binaryResults(p0, p1)(resultContext, advance(paramResults)) })
        }
        // binaryResults only supports two results for two parameters
        case _ => throw new IllegalArgumentException("unexpected parameter count for binary results")
      }
    }
    
    def ternaryResults(p0: RuleParam, p1: RuleParam, p2: RuleParam)
                      (resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]])
                      (implicit rule: Rule): ProofResult = {
      paramResults match {
        case Seq(r0, r1, r2) => (filterSuccess(r0, p0.goal), filterSuccess(r1, p1.goal), filterSuccess(r2, p2.goal)) match {
          case (f0, f1, f2) if f0.isEmpty || f0.isEmpty => failure()
          case (Stream(head0), Stream(head1), Stream(head2)) => success(inferFromResults(head0, head1, head2))
          case (f0, f1, f2) => 
            success(inferFromResults(f0.head, f1.head, f2.head), { ternaryResults(p0, p1, p2)(resultContext, advance(paramResults)) })
        }
        // ternaryResults only supports three results for two parameters
        case _ => throw new IllegalArgumentException("unexpected parameter count for binary results")
      }
    }
    
    def optionResults(resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]]): ProofResult = paramResults match {
      case Seq(res) => res.head
      case _ => throw new IllegalArgumentException("unexpected parameter count for binary results")
    }
    
    // ------------------------- //
    
    // pendingParams creates a new 'Pending' instance for the given RuleParams and Rule
    def pendingParams(params: RuleParams)(implicit rule: Rule): Pending = params match {
      case UnaryParams(param0) => pending(unaryResults(param0), step { tryParam(param0) })
      case BinaryParams(param0, param1) => pending(binaryResults(param0, param1), step { tryParam(param0) }, step { tryParam(param1) })
      case TernaryParams(param0, param1, param2) =>
        pending(ternaryResults(param0, param1, param2),
                step { tryParam(param0) },
                step { tryParam(param1) },
                step { tryParam(param2) })
      case OptionParams(opts @ _*) => {
        pending(optionResults, opts.map {
          params => step({ pendingParams(params) })
        }.reduce((acc, step) => acc.then(step)))
      }
      // TODO Make sure EmptyParams/EmptyArgs is actually necessary; I can't remember why they were added. ¯\(°_o)/¯
      case EmptyParams() => throw new RuntimeException("infer returned invalid parameters")
    }
    
    rules match {
      case RuleSet(Nil) => failure()
      case RuleSet(Seq(head, rem @ _*)) => infer(head, args) match {
        case Left(proof:NullProof) => failure(inferFrom(RuleSet(rem), args))
        case Left(proof:CompleteProof) => success(proof, inferFrom(RuleSet(rem), args))
        case Right(params) => pendingParams(params)(head)
      }
    }
  }
  
  private def tryParam(param: RuleParam)(implicit context: ProofContext): ProofResult = param match {
    case EmptyProof(conc) => success(ProudPremise(conc).proof)
    case AnyProof(conc) => {
      implicit val newContext = context.withGoal(conc)
      proof(strategy.premises(newContext))(newContext)
    }
    case RelevantProof(conc, discharges, restrict@_*) => {
      implicit val newContext = context.withGoal(conc).withAssumptions(discharges.assumptions:_*).lessPremises(restrict:_*)
      proof(strategy.premises(newContext))(newContext)
    }
  }
  
  private def infer(rule: Rule, args: RuleArgs = EmptyArgs())(implicit context: ProofContext): Either[Proof, RuleParams] = {
    rule.infer(context.goal)(args) match {
      // case for trivial (tautological) proof
      case CompleteResult(proof) => Left(proof)
      // case for decomposing rule requirements
      case IncompleteResult(params) => Right(params)
      // case for non-applicable rules
      case NullResult() => Left(NullProof())
    }
  }

  ///////// Utility Methods ///////////
  
  // convenience method that creates a new ProofStep instance from a call-by-name parameter
  private def step(stepFunc: => ProofResult)(implicit context: ProofContext) = ProofStep { stepFunc }
  
  private def success(proof: CompleteProof, hint: SearchHint)(implicit context: ProofContext) = {
    strategy.decide(Success(proof, context, hint))
  }
  
  private def success(proof: CompleteProof, continuation: => ProofResult)(implicit context: ProofContext) = {
    strategy.decide(Success(proof, context, Continue(step(continuation))))
  }
  
  private def success(proof: CompleteProof)(implicit context: ProofContext) = strategy.decide(Success(proof, context, Cut()))
  
  private def failure(hint: SearchHint)(implicit context: ProofContext) = strategy.decide(Failure(context, hint))
  
  private def failure(continuation: => ProofResult)(implicit context: ProofContext) = {
    strategy.decide(Failure(context, Continue(step(continuation))))
  }
  
  private def failure()(implicit context: ProofContext) = strategy.decide(Failure(context, Cut()))
  
  private def pending(aggregator: (ProofContext, Seq[Stream[ProofResult]]) => ProofResult,
                      steps: ProofStep*)(implicit context: ProofContext) = {
    Pending(context, immutable(steps), aggregator)
  }
  
  private def immutable[T](seq: scala.Seq[T]) = scala.collection.immutable.Seq(seq:_*)
  
//  private def then(result: ProofResult, continue: () => Proof = null): Proof = result match {
//    case Failure(proof, Continue()) if continue != null => continue()
//    case finish => finish.proof
//  }
//  
//  private def proofFromPremises(premises: Seq[Premise])(implicit context: ProofContext): Proof = premises match {
//    case Nil => proofFromInference(RuleSet(strategy.rules))
//    case Seq(head, rem@_*) => head match {
//      case ProudPremise(s) if s.matches(context.goal) => ProudPremise(s).proof
//      case Assumption(s) if s.matches(context.goal) => Assumption(s).proof
//      case Conclusion(s, rule, args) if s.matches(context.goal) => args match {
//        case EmptyArgs() => CompleteProof(s, rule, args, Nil)
//        case UnaryArgs(proof0) => CompleteProof(s, rule, args, proof0.premises)
//        case BinaryArgs(proof0, proof1) => CompleteProof(s, rule, args, proof0.premises ++ proof1.premises)
//        case TernaryArgs(proof0, proof1, proof2) => CompleteProof(s, rule, args, proof0.premises ++ proof1.premises ++ proof2.premises)
//      }
//      case premise if premise.sentence.contains(context.goal) =>
//        // if the premise contains our goal, attempt to infer conclusion from a rule that accepts the sentence as a major premise
//        val proudProof = ProudPremise(premise.sentence).proof
//        val relevantRules = strategy.rules(context.withRuleSet(context.rules.acceptingMajor(proudProof)))
//        proofFromInference(relevantRules, UnaryArgs(proudProof)) match {
//          case NullProof(_) => proofFromPremises(rem)
//          case proof => proof
//        }
//      case premise if context.goal.equals(Absurdity()) =>
//        val relevantRules = strategy.rules(context.withRuleSet(context.rules.yielding(context.goal)))
//        println(premise)
//        println(relevantRules)
//        proofFromInference(relevantRules) match {
//          case NullProof(_) => proofFromPremises(rem)
//          case proof => proof
//        }
//      case _ => proofFromPremises(rem)
//    }
//  }
//  
//  private def proofFromInference(rules: RuleSet, args: RuleArgs = EmptyArgs())(implicit context: ProofContext): Proof = rules match {
//    case RuleSet(Nil) => NullProof(context.premises)
//    case RuleSet(Seq(head, rem@_*)) => infer(head, args) match {
//      case Left(NullProof(prems)) => proofFromInference(RuleSet(rem), args)
//      case Left(proof) => proof
//      case Right(params) => infer(head, proof(params)) match {
//        case Left(CompleteProof(c, p)) => CompleteProof(c, p)
//        case Left(NullProof(_)) | Right(_) => proofFromInference(RuleSet(rem), args)
//      }
//    }
//  }
//  
//  private def relevantProof(discharge: Discharge, restrict: Seq[Assumption])(implicit context: ProofContext): Proof = {
//      val newContext = context.lessAssumptions(restrict:_*)
//      discharge match {
//        case Vacuous(assumptions@_*) => then(proof(newContext.withAssumptions(assumptions:_*)))
//        case Required(assumptions@_*) => then(proof(newContext.withAssumptions(assumptions:_*))) match {
//          case CompleteProof(c, prems) if assumptions.par forall { a => prems exists ( p => p.matches(a.sentence)) } => CompleteProof(c, (prems ++ context.premises).distinct)
//          case _ => NullProof(newContext.premises)
//        }
//        case Variate(assumptions@_*) => then(proof(newContext.withAssumptions(assumptions:_*))) match {
//          case CompleteProof(c, prems) if prems.par exists { p => assumptions exists { a => a.sentence.matches(p.sentence) } } =>
//            CompleteProof(c, (prems ++ context.premises).distinct)
//          case _ => NullProof(newContext.premises)
//        }
//      }
//  }
//
//  private def proof(param: RuleParam)(implicit context: ProofContext): Proof = param match {
//    case EmptyProof(conc) => ProudPremise(conc).proof
//    case AnyProof(conc) => then(proof(context.withGoal(conc)))
//    case RelevantProof(conc, discharge, restrict@_*) => relevantProof(discharge, restrict)(context.withGoal(conc))
//    case _ => NullProof(context.premises)
//  }
//
//  private def proof(params: RuleParams)(implicit context: ProofContext): RuleArgs = params match {
//    case EmptyParams() => EmptyArgs()
//    case UnaryParams(p0) => UnaryArgs(proof(p0))
//    case BinaryParams(p0, p1) => BinaryArgs(proof(p0), proof(p1))
//    case TernaryParams(p0, p1, p2) => TernaryArgs(proof(p0), proof(p1), proof(p2))
//    // TODO fixme: good example of a need for better IoC
//    case OptionParams(all@_*) => all.map(p => proof(p)).find { arg => arg.prems forall { case CompleteProof(_,_) => true; case _ => false; }}.getOrElse(EmptyArgs())
//  }
//
}