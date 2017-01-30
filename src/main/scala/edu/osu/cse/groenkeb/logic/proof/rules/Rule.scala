package edu.osu.cse.groenkeb.logic.proof.rules

import scala.collection.immutable.LinearSeq

import edu.osu.cse.groenkeb.logic.Relation

// need some way of defining a rule to take some number of sentences
// and mapping them to relations; rules should operate on relations, not operators!
// idea: RelationMap .... maps sentences => relational-nodes, and stores relational-nodes
// in a tree according to their appearances in the sentences. A rule could then locate the
// relevant relations for the given sentences and filter out those that are not applicable

trait Rule
{
  def apply(relations: LinearSeq[Relation]): LinearSeq[Relation]
}

abstract class IntroductionRule extends Rule
{
  def invert(): EliminationRule
}

abstract class EliminationRule extends Rule
{
  def invert(): IntroductionRule
}
