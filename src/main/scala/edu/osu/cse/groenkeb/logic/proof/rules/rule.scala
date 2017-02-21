package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Turnstile
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.SentenceRelation
import edu.osu.cse.groenkeb.logic.Relation

trait Rule {
  /**
   * The "parity" of this rule; must be either elimination, introduction, or none.
   */
  def parity: RuleParity

  /**
   * True if this rule accepts the given object type as a premise, false otherwise.
   */
  def accepts(relation: Relation): Boolean

  /**
   * True if this rule yields the given object type as a conclusion, false otherwise.
   */
  def yields(relation: Relation): Boolean

  /**
   *
   */
  def infer(conclusion: ObjectRelation)(from: Relation*): InferenceResult
}

abstract class BaseRule extends Rule

case class AndIntroRule() extends BaseRule {
  def parity = Introduction(AndElimRule())

  def accepts(relation: Relation) = relation match {
    case SentenceRelation(x) => true
    case _ => false
  }

  def yields(relation: Relation) = relation match {
    case And(x, y) => true
    case _ => false
  }

  def infer(conclusion: ObjectRelation)(from: Relation*) = conclusion match {
    case And(x, y) => from match {
      // NOTE: order matters for this match; will need to repeat cases to handle differing order if this is an issue
      case Seq(SentenceRelation(x), SentenceRelation(y)) => CompleteResult(And(x, y))
      case Seq(SentenceRelation(x)) => IncompleteResult(Require(SentenceRelation(y)))
      case Seq(SentenceRelation(y)) => IncompleteResult(Require(SentenceRelation(x)))
      case _ => IncompleteResult(Composite(Require(x.toRelation), Require(y.toRelation)))
    }
    case _ => NullResult()
  }
}

case class AndElimRule() extends BaseRule {
  def parity = Elimination(AndIntroRule())

  def accepts(relation: Relation) = relation match {
    case And(x, y) => true
    case Turnstile(s, y) => true
    case _ => false
  }

  def yields(relation: Relation) = relation match {
    case SentenceRelation(x) => true
    case _ => false
  }

  def infer(conclusion: ObjectRelation)(from: Relation*) = {
    conclusion match {
      case SentenceRelation(c) => from match {
        case Seq(And(x, y), Turnstile(s, c)) if s.contains(x) => CompleteResult(SentenceRelation(c))
        case Seq(And(x, y), Turnstile(s, c)) if s.contains(y) => CompleteResult(SentenceRelation(c))
        case Seq(And(x, y), _*) => 
          IncompleteResult(Discrete(Vacuous(Turnstile(List(x), c)), Vacuous(Turnstile(List(y), c))))
        case _ => NullResult()
      }
      case _ => NullResult()
    }
  }
}

case class ReflexivityRule() extends BaseRule {
  def parity = None(this)
  
  def accepts(relation: Relation) = relation match {
    case SentenceRelation(x) => true
    case _ => false
  }
  
  def yields(relation: Relation) = relation match {
    case SentenceRelation(x) => true
    case _ => false
  }
  
  def infer(conclusion: ObjectRelation)(from: Relation*) = {
    conclusion match {
      case SentenceRelation(x) => from match {
        case Seq(SentenceRelation(x)) => CompleteResult(SentenceRelation(x))
        case _ => IncompleteResult(Require(SentenceRelation(x)))
      }
      case _ => NullResult()
    }
  }
}

case class NullRule() extends BaseRule {
  def parity = None(this)

  def accepts(relation: Relation) = false

  def yields(relation: Relation) = false

  def infer(conclusion: ObjectRelation)(from: Relation*) = NullResult()
}

