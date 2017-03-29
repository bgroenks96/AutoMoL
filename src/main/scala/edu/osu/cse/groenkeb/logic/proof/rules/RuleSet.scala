package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.Proof

case class RuleSet private(val rules: Seq[Rule]) extends Seq[Rule] {
  def this(rules: Set[Rule]) = this(rules.toSeq)
  
  def iterator = rules.iterator
  
  def apply(idx: Int) = rules.apply(idx)
  
  def length = rules.length
  
  def yielding(sentence: Sentence) = {
    subset { r => r.yields(sentence) }
  }

  def accepting(proof: Proof) = {
    subset { r => r.accepts(proof) }
  }

  def subset(predicate: Rule => Boolean) = new RuleSet(rules.filter { predicate })
  
  def without(rule: Rule) = subset { r => r.equals(rule) }
}

object RuleSet {
  def apply(rules: Set[Rule]) = new RuleSet(rules.toSeq)
}