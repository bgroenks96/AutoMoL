package edu.osu.cse.groenkeb.logic.proof.rules

import scala.collection.immutable.LinearSeq

import edu.osu.cse.groenkeb.logic._

trait Rule
{
  def apply(relations: List[ObjectRelation]): List[ObjectRelation]
  
  def accepts(relations: List[ObjectRelation]): Boolean
}

sealed abstract class BaseRule extends Rule
{
  def accepts(relations: List[ObjectRelation]): Boolean = apply(relations).size > 0
}

sealed abstract class IntroductionRule extends BaseRule
{
  def invert(): EliminationRule
}

sealed abstract class EliminationRule extends BaseRule
{
  def invert(): IntroductionRule
}

case class AndIntroRule() extends IntroductionRule
{
  def invert(): EliminationRule = AndElimRule()
  
  def apply(relations: List[ObjectRelation]): List[ObjectRelation] = relations match
  {
    case SentenceRelation(x) :: SentenceRelation(y) :: Nil => List(AndRelation(x, y))
    case _ => List()
  }
}

case class AndElimRule() extends EliminationRule
{
  def invert(): IntroductionRule = AndIntroRule()
  
  def apply(relations: List[ObjectRelation]): List[ObjectRelation] = relations match
  {
    case AndRelation(x, y) :: Nil => List(SentenceRelation(x), SentenceRelation(y))
    case _ => List()
  }
}

case class NullRule() extends BaseRule
{
  def apply(relations: List[ObjectRelation]): List[ObjectRelation] = List()
}

