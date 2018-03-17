package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.model.rules._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.engine.ProofResult
import edu.osu.cse.groenkeb.logic.proof.engine.ProofStrategy

import scala.collection.immutable.Seq

final class EvaluationProofStrategy extends ProofStrategy {

  def rules(implicit context: ProofContext): RuleSet = context.goal match {
    case Absurdity => falsificationRules
    case _ => verificationRules
  }
  
  def premises(implicit context: ProofContext): Seq[Premise] = Seq(context.available.toSeq:_*)
  
  def decide(result: ProofResult)(implicit context: ProofContext): ProofResult = result
  
  private def verificationRules(implicit context: ProofContext) = context.goal match {
    case AtomicSentence(a) => context.rules.subset { r => r.isInstanceOf[ModelRule] }
    case Not(s) => context.rules.subset { r => r == NegationVerification }
    case And(left, right) => context.rules.subset { r => r == AndVerification }
    case Or(left, right) => context.rules.subset { r => r == OrVerification }
    case If(left, right) => context.rules.subset { r => r == ConditionalVerification }
    case ForAll(term, sentence) => context.rules.subset { r => r.isInstanceOf[UniversalVerification] }
    case Exists(term, sentence) => context.rules.subset { r => r.isInstanceOf[ExistentialVerification] }
    case _ => context.rules
  }
  
  private def falsificationRules(implicit context: ProofContext) = RuleSet(context.available.flatMap {
    a => a.sentence match {
      case AtomicSentence(a) => context.rules.subset { r => r.isInstanceOf[ModelRule] }
      case Not(s) => context.rules.subset { r => r == NegationFalsification }
      case And(left, right) => context.rules.subset { r => r == AndFalsification }
      case Or(left, right) => context.rules.subset { r => r == OrFalsification }
      case If(left, right) => context.rules.subset { r => r == ConditionalFalsification }
      case ForAll(term, sentence) => context.rules.subset { r => r.isInstanceOf[UniversalFalsification] }
      case Exists(term, sentence) => context.rules.subset { r => r.isInstanceOf[ExistentialFalsification] }
      case _ => Nil
    }
  })
}
