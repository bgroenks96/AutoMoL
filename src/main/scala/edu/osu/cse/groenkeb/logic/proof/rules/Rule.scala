package edu.osu.cse.groenkeb.logic.proof.rules

import scala.collection.immutable.LinearSeq

import edu.osu.cse.groenkeb.logic._

// need some way of defining a rule to take some number of sentences
// and mapping them to relations; rules should operate on relations, not operators!
// idea: RelationMap .... maps sentences => relational-nodes, and stores relational-nodes
// in a tree according to their appearances in the sentences. A rule could then locate the
// relevant relations for the given sentences and filter out those that are not applicable

trait Rule
{
  def apply(relations: List[Relation]): List[Relation]
  
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
  
  def apply(relations: List[Relation]): List[Relation] = relations match
  {
    case TruthRelation(x) :: TruthRelation(y) :: Nil => List(AndRelation(x.member, y.member))
    case _ => List()
  }
}

case class AndElimRule() extends EliminationRule
{
  def invert(): IntroductionRule = AndIntroRule()
  
  def apply(relations: List[Relation]): List[Relation] = relations match
  {
    case AndRelation(x, y) :: Nil => List(TruthRelation(x.relate), TruthRelation(y.relate))
    case _ => List()
  }
}
