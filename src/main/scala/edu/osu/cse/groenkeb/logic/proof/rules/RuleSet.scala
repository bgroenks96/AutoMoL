package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.ObjectRelation

case class RuleSet(val rules: Rule*) {
  def yielding(relation: ObjectRelation) = {
    subset { r => r.yields(relation) }
  }
  
  def accepting(relation: ObjectRelation) = {
    subset { r => r.accepts(relation) }
  }
  
  def subset(predicate: Rule => Boolean) = {
    RuleSet(rules.filter { predicate }:_*)
  }
}