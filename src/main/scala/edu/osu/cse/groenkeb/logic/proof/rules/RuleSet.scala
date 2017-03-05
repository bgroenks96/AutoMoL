package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.ObjectRelation

case class RuleSet(val rules: Rule*) extends Seq[Rule] {
  def iterator = rules.iterator
  
  def apply(idx: Int) = rules.apply(idx)
  
  def length = rules.length
  
  def yielding(obj: ObjectRelation) = {
    subset { r => r.yields(obj) }
  }

  def accepting(proof: Proof) = {
    subset { r => r.accepts(proof) }
  }

  def subset(predicate: Rule => Boolean) = {
    RuleSet(rules.filter { predicate }: _*)
  }
}