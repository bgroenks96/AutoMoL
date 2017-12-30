package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.Proof

import scala.collection.immutable.Set
import scala.collection.immutable.Seq

case class RuleSet(val rules: Seq[Rule]) extends Seq[Rule] {
  def this(rules: Set[Rule]) = this(Seq(rules.toSeq:_*))
  def this(rules: RuleSet) = this(rules.rules)
  
  def iterator = rules.iterator
  
  def apply(idx: Int) = rules.apply(idx)
  
  def length = rules.length
  
  def yielding(sentence: Sentence) = {
    subset { r => r.yields(sentence) }
  }

  def subset(predicate: Rule => Boolean) = RuleSet(rules.filter { predicate })
  
  def without(rule: Rule) = subset { r => r.equals(rule) }
}

object RuleSet {
  def apply(rules: Set[Rule]) = new RuleSet(Seq(rules.toSeq:_*))
  def apply(rules: RuleSet) = new RuleSet(rules.rules)
}