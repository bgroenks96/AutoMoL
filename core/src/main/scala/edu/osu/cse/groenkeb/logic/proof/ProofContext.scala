package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._

import scala.collection.immutable.Seq
import scala.collection.immutable.Set

object ProofContext {
  def apply(goal: Sentence,
            rules: RuleSet,
            available: Traversable[Premise]) = new ProofContext(goal, rules, available)
  
  def apply(goal: Sentence, available: Traversable[Premise] = Nil)(implicit rules: RuleSet) = new ProofContext(goal, available)
}

case class ProofContext (goal: Sentence,
                         rule: Rule,
                         rules: RuleSet,
                         available: Set[Premise],
                         depth: Int,
                         parent: Option[ProofContext]) {
  def this(goal: Sentence, rules: RuleSet, available: Traversable[Premise]) = this(goal, NullRule, rules, available.toSet, 1, None)
  
  def this(goal: Sentence, available: Traversable[Premise] = Nil)(implicit rules: RuleSet) = this(goal, rules, available)
  
  def withAssumptions(newAssumptions: Assumption*) = ProofContext(goal, rule, rules, merge(available, newAssumptions), depth, parent)
  
  def withGoal(newGoal: Sentence, newRule: Rule) = ProofContext(newGoal, newRule, rules, available, depth + 1, Some(this))
  
  def restrict(premises: Premise*) = ProofContext(goal, rule, rules, available.filterNot { p => premises.exists { r => r.matches(p) } }, depth, parent)
  
  def fromAvailable(sentence: Sentence) = available.find { p => p.matches(sentence) }
  
  def hasGoal(sentence: Sentence) = goal.matches(sentence)
  
  override def toString = "ProofContext(%s, %s@%d, {%s})".format(goal, rule, depth, available.mkString(","))
  
  private def merge[T <: Premise](a: Traversable[Premise], b: Traversable[T]) = 
    (a.filterNot { s => b.exists { n => n.matches(s) } } ++ b).toSet
}
