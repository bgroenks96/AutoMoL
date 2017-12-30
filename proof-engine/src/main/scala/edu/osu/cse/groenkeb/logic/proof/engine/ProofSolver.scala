package edu.osu.cse.groenkeb.logic.proof.engine

import scala.Left
import scala.Right
import scala.collection.immutable.Nil
import scala.collection.immutable.Seq
import scala.collection.immutable.Set
import scala.collection.immutable.Stream

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof._

sealed abstract class SolverOpts
case object Trace extends SolverOpts

final class ProofSolver(implicit strategy: ProofStrategy = new NaiveProofStrategy(),
                        options: Seq[SolverOpts] = Nil) {
  
  def prove(context: ProofContext): Stream[ProofResult] = {
    implicit val cntxt = context
    ProofSearch(step { proof(strategy.premises) } )()
  }

  private def proof(premises: scala.Seq[Premise])(implicit context: ProofContext): ProofResult = {
    if (trace) println(context)
    premises match {
      case Nil => inferFrom(RuleSet(strategy.rules))
      case Seq(head, rem @ _*) => head match {
        case a: Assumption if context.hasGoal(a.sentence) => success(a.proof, proof(rem))
        case p @ Proof(s, rule, args, undischarged, _) if context.hasGoal(s) => success(p, { proof(rem) })
        case _ => proof(rem)
      }
    }
  }

  private def inferFrom(rules: RuleSet)(implicit context: ProofContext): ProofResult = {
    // inferFromResults matches the given sequence of Success results to a RuleArgs type and performs the final inference
    // step. It is assumed that the given result has already been validated to fulfill the necessary inference requirements.
    // Failure to meet this precondition is an error on the part of the caller.
    def inferFromUnaryResult(res0: Success)(implicit rule: Rule): Proof =
      rule.infer(UnaryArgs(res0.proof)) match {
        case Some(proof) => proof
        // Exceptional case: one or more of the supplied Success results failed to satisfy the precondition.
        // This would indicate a bug in either the calling code or in the rule.
        case None => throw new IllegalArgumentException("one or more arguments failed to satisfy the rule [%s]: %s".format(rule, res0))
      }

    def inferFromBinaryResults(res0: Success, res1: Success)(implicit rule: Rule): Proof =
      rule.infer(BinaryArgs(res0.proof, res1.proof)) match {
        case Some(proof) => proof
        case None => throw new IllegalArgumentException("one or more arguments failed to satisfy the rule [%s]: %s %s".format(rule, res0, res1))
      }

    def inferFromTernaryResults(res0: Success, res1: Success, res2: Success)(implicit rule: Rule): Proof =
      rule.infer(TernaryArgs(res0.proof, res1.proof, res2.proof)) match {
        case Some(proof) => proof
        case None => throw new IllegalArgumentException("one or more arguments failed to satisfy the rule [%s]: %s %s %s".format(rule, res0, res1, res2))
      }
    
    def inferFromNResults(resN: Seq[Success])(implicit rule: Rule): Proof =
      rule.infer(NArgs(resN.map { s => s.proof })) match {
        case Some(proof) => proof
        case None => throw new IllegalArgumentException("one or more arguments failed to satisfy the rule [%s]: %s".format(rule, resN))
      }

    // onlyValid filters the given proof results to include only those that are both successful and 'valid', i.e. satisfy
    // the specified goal as well as the requirements for discharge.
    def onlyValid(results: Stream[ProofResult], param: RuleParam): Stream[Success] = {
      def isValid(proof: Proof) = param match {
        case EmptyProof(c) => c.matches(param.goal)
        case AnyProof(c) => c.matches(param.goal)
        case RelevantProof(c, d, r@_*) => d match {
          case Required(assumptions@_*) => proof.conclusion.matches(c) && 
                                           d.assumptions.forall { a => proof.undischarged.exists { p => p.matches(a) } }
          case Vacuous(assumptions@_*) => proof.conclusion.matches(c)
          case Variate(assumptions@_*) => proof.conclusion.matches(c) &&
                                          d.assumptions.exists { a => proof.undischarged.exists { p => p.matches(a) } }
        }
      }
      
      results.collect { case res:Success if isValid(res.proof) => res }
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
        case Seq(results0) => onlyValid(results0, p0) match {
          case Stream() => failure()
          case Stream(head, rem@_*) => success(inferFromUnaryResult(head), { unaryResults(p0)(resultContext, Seq(rem.toStream)) })
        }
        // unaryResults only supports a single set of results for a single parameter
        case _ => throw new IllegalArgumentException("unexpected parameter count for unary results")
      }


    def binaryResults(p0: RuleParam, p1: RuleParam)(resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]])
                     (implicit rule: Rule): ProofResult = {
      paramResults match {
        case Seq(results0, results1) => (onlyValid(results0, p0), onlyValid(results1, p1)) match {
          case (filtered0, filtered1) if filtered0.isEmpty || filtered1.isEmpty => failure()
          case (filtered0, filtered1) =>
            success(inferFromBinaryResults(filtered0.head, filtered1.head), { binaryResults(p0, p1)(resultContext, advance(paramResults)) })
        }
        // binaryResults only supports two results for two parameters
        case _ => throw new IllegalArgumentException("unexpected parameter count for binary results")
      }
    }

    def ternaryResults(p0: RuleParam, p1: RuleParam, p2: RuleParam)
                      (resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]])
                      (implicit rule: Rule): ProofResult = {
      paramResults match {
        case Seq(r0, r1, r2) => (onlyValid(r0, p0), onlyValid(r1, p1), onlyValid(r2, p2)) match {
          case (f0, f1, f2) if f0.isEmpty || f1.isEmpty || f2.isEmpty => failure()
          case (f0, f1, f2) =>
            success(inferFromTernaryResults(f0.head, f1.head, f2.head), { ternaryResults(p0, p1, p2)(resultContext, advance(paramResults)) })
        }
        // ternaryResults only supports three results for two parameters
        case _ => throw new IllegalArgumentException("unexpected parameter count for binary results")
      }
    }
    
    def nResults(params: Seq[RuleParam])
                (resultContext: ProofContext, paramResults: Seq[Stream[ProofResult]])(implicit rule: Rule): ProofResult = {
      val filtered = for ((res, p) <- (paramResults zip params)) yield onlyValid(res, p)
      filtered match {
        case anyMissing if anyMissing.length < params.length => failure()
        case all if all.forall { res => !res.isEmpty } =>
          success(inferFromNResults(filtered.map { r => r.head }),
          { nResults(params)(resultContext, advance(paramResults)) })
        case _ => failure()
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
      case NParams(params) => pending(nResults(immutable(params)), params.map { p => step { tryParam(p) }}:_*)
      case OptionParams(opts @ _*) =>
        pending(optionResults, opts.map {
          params => step({ pendingParams(params) })
        }.reduce((acc, step) => acc.then(step)))
      case EmptyParams => rule.infer(EmptyArgs) match {
        case None => pending((c, r) => r.head.head, step { failure() } )
        case Some(proof) => pending((c, r) => r.head.head, step { success(proof) })
      }
    }

    rules match {
      case RuleSet(Nil) => failure()
      case RuleSet(Seq(head, rem @ _*)) => 
        def tryNext(rule: Rule, majors: Seq[Option[Sentence]]): ProofResult = majors match {
          case Nil => failure({ inferFrom(RuleSet(immutable(rem))) })
          case Seq(head, rem @ _*) => rule.params(head) match {
            case None => failure({ tryNext(rule, immutable(rem)) })
            case Some(params) => pendingParams(params)(rule).andThen(step({
              tryNext(rule, immutable(rem))
            }))
          }
        }
        
        tryNext(head, Seq(None) ++ (strategy.premises map { p => Some(p.sentence) }))
    }
  }

  private def tryParam(param: RuleParam)(implicit rule: Rule, context: ProofContext): ProofResult = param match {
    case EmptyProof(conc) => context.available.collect({ case a:Assumption => a }).find { a => a.matches(conc) } match {
      case Some(assumption) => success(assumption.proof)
      case None => failure()
    }
    case AnyProof(conc) => {
      val newContext = context.withGoal(conc, rule)
      proof(strategy.premises(newContext))(newContext)
    }
    case RelevantProof(conc, discharges, restrict@_*) => {
      val newContext = context.withGoal(conc, rule).withAssumptions(discharges.assumptions:_*).restrict(restrict:_*)
      proof(strategy.premises(newContext))(newContext)
    }
  }
  
  private def trace = this.options.contains(Trace)

  ///////// Utility Methods ///////////

  // convenience method that creates a new ProofStep instance from a call-by-name parameter
  private def step(stepFunc: => ProofResult)(implicit context: ProofContext) = ProofStep { stepFunc }

  private def success(proof: Proof, hint: SearchHint)(implicit context: ProofContext) = {
    strategy.decide(Success(proof, context, hint))
  }

  private def success(proof: Proof, continuation: => ProofResult)(implicit context: ProofContext) = {
    strategy.decide(Success(proof, context, Continue(step(continuation))))
  }

  private def success(proof: Proof)(implicit context: ProofContext) = strategy.decide(Success(proof, context, Cut()))

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
}
