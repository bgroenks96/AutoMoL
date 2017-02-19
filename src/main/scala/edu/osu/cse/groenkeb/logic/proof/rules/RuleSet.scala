package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.ObjectRelation

trait RuleSet {
  def rules: Seq[Rule]
  def yielding(relation: ObjectRelation): RuleSet
}