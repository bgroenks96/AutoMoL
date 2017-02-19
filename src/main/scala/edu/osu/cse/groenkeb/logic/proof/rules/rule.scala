package edu.osu.cse.groenkeb.logic.proof.rules

import scala.collection.immutable.LinearSeq

import edu.osu.cse.groenkeb.logic._

trait Rule
{
  /**
   * The "parity" of this rule; must be either elimination, introduction, or none.
   */
  def parity: RuleParity
  
  /**
   * True if this rule accepts the given object type as a premise, false otherwise.
   */
  def accepts(relation: ObjectRelation): Boolean
  
  /**
   * True if this rule yields the given object type as a conclusion, false otherwise.
   */
  def yields(relation: ObjectRelation): Boolean
  
  /**
   * 
   */
  def infer(conclusion: ObjectRelation)(from: ObjectRelation*): InferenceResult
}

sealed abstract class RuleParity(inverse: Rule)
case class Elimination(inverse: Rule) extends RuleParity(inverse)
case class Introduction(inverse: Rule) extends RuleParity(inverse)
case class None() extends RuleParity(NullRule())

sealed abstract class InferenceResult
case class CompleteResult(val conclusions: ObjectRelation*) extends InferenceResult
case class IncompleteResult(val required: ObjectRelation*) extends InferenceResult
case class NullResult() extends InferenceResult

abstract class BaseRule extends Rule

case class AndIntroRule() extends BaseRule
{
  def parity = Introduction(AndElimRule())
  
  def accepts(relation: ObjectRelation) = relation match {
    case SentenceRelation(x) => true
    case _ => false
  }
  
  def yields(relation: ObjectRelation) = relation match {
    case AndRelation(x, y) => true
    case _ => false
  }
  
  def infer(conclusion: ObjectRelation)(from: ObjectRelation*) = conclusion match {
    case AndRelation(x, y) => from match {
      // NOTE: order matters for this match; will need to repeat cases to handle differing order if this is an issue
      case Seq(SentenceRelation(x), SentenceRelation(y)) => CompleteResult(AndRelation(x, y))
      case Seq(SentenceRelation(x)) => IncompleteResult(SentenceRelation(y))
      case Seq(SentenceRelation(y)) => IncompleteResult(SentenceRelation(x))
      case _ => IncompleteResult(SentenceRelation(x), SentenceRelation(y))
    }
    case _ => NullResult()
  }
}

case class AndElimRule() extends BaseRule
{
  def parity = Elimination(AndIntroRule())
  
  def accepts(relation: ObjectRelation) = relation match {
    case AndRelation(x, y) => true
    case _ => false
  }
  
  def yields(relation: ObjectRelation) = relation match {
    case SentenceRelation(x) => true
    case _ => false
  }
  
  def infer(conclusion: ObjectRelation)(from: ObjectRelation*) = {
    conclusion match {
      case SentenceRelation(x) => from match {
        case Seq(AndRelation(x, y)) => CompleteResult(SentenceRelation(x))
        case Seq(AndRelation(y, x)) => CompleteResult(SentenceRelation(x))
        case _ => NullResult()
      }
      case _ => NullResult()
    }
  }
}

case class NullRule() extends BaseRule
{
  def parity = None()
  
  def accepts(relation: ObjectRelation) = false
  
  def yields(relation: ObjectRelation) = false
  
  def infer(conclusion: ObjectRelation)(from: ObjectRelation*) = NullResult()
}

