package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.utils.Empty

abstract class BaseRule extends Rule {
  def goal(implicit context: ProofContext) = context.goal
  
  implicit def sentenceExtensions(s: Sentence) = new {
    def is(arg: Sentence) = s == arg
  }
  
  implicit def assumptionSetExtensions(assumptions: Set[Assumption]) = new {
    def discharge(sentences: Sentence*) = assumptions filterNot { a => sentences.exists { s => a.matches(s) }}
  }
  
  implicit def booleanExtensions(b: Boolean) = new {
    def and(arg: Boolean) = b && arg
    def or(arg: Boolean) = b || arg
    def not = !b
  }
}

final case object IdentityRule extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = true
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = Some(UnaryParams(AnyProof(goal)))

  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case UnaryArgs(Proof(context.goal, _, _, Empty())) =>
      Some(Proof(context.goal, this, args, Set(Assumption(context.goal))))
    case _ => None
  }

  override def toString = "id"
}

final case object NonContradictionRule extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = sentence is Absurdity
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = Some(BinaryParams(AnyProof(goal), AnyProof(Not(goal))))
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = args match {
    case BinaryArgs(Proof(context.goal,_,_, pa),
                    Proof(Not(context.goal),_,_, pb)) =>
                      Some(Proof(Absurdity, this, args, pa ++ pb))
    case _ => None
  }
  
  override def toString = "NC"
}

final case object NullRule extends BaseRule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false

  def params(major: Option[Sentence])(implicit context: ProofContext) = None
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = None

  override def toString = "nil"
}

