package edu.osu.cse.groenkeb.logic.proof.rules

import scala.collection.immutable.LinearSeq

import edu.osu.cse.groenkeb.logic._
import RuleUtils._

trait Rule
{
  def apply(relations: List[Relation]): List[MetaRelation]
  
  def accepts(relations: List[Relation]): Boolean
}

abstract class BaseRule extends Rule
{
  def accepts(relations: List[Relation]): Boolean = apply(relations).size > 0
}

abstract class IntroductionRule extends BaseRule
{
  def invert(): EliminationRule
}

abstract class EliminationRule extends BaseRule
{
  def invert(): IntroductionRule
}

case class AndIntroRule() extends IntroductionRule
{
  def invert(): EliminationRule = AndElimRule()
  
  def apply(relations: List[Relation]): List[MetaRelation] = relations match
  {
    case TruthRelation(x) :: TruthRelation(y) :: Nil => List(confirm(AndRelation(x.member, y.member).result))
    case _ => List()
  }
}

case class AndElimRule() extends EliminationRule
{
  def invert(): IntroductionRule = AndIntroRule()
  
  def apply(relations: List[Relation]): List[MetaRelation] = relations match
  {
    case AndRelation(x, y) :: Nil => List(TruthRelation(x.relate), TruthRelation(y.relate))
    case _ => List()
  }
}

private object RuleUtils
{
  def confirm(s: SentenceRelation): TruthRelation = TruthRelation(s);
  
  def reject(s: SentenceRelation): AbsurdityRelation = AbsurdityRelation(s);
}

