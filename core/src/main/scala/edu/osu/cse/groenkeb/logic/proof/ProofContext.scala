package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

object ProofContext {
  def apply(goal: Sentence,
            rules: RuleSet,
            premises: Seq[Premise]) = new ProofContext(goal, rules, premises.toSet)
  
  def apply(goal: Sentence, premises: Seq[Premise] = Nil)(implicit rules: RuleSet) = new ProofContext(goal, rules, premises.toSet)
}

case class ProofContext (val goal: Sentence,
                                 val rules: RuleSet,
                                 val premises: Set[Premise]) {
  
  def withAssumptions(newAssumptions: Assumption*) = ProofContext(goal, rules, premises ++ newAssumptions)
  
  def withRuleSet(newRules: RuleSet) = ProofContext(goal, newRules, premises)
  
  def withGoal(newGoal: Sentence) = ProofContext(newGoal, rules, premises)
  
  def lessPremises(restrict: Premise*) = ProofContext(goal, rules, premises.filterNot { p => restrict.exists { r => r.matches(p) } })
  
  def has(premise: Premise) = premises.exists { p => p.matches(premise) }
  
  def hasGoal(sentence: Sentence) = goal.matches(sentence)
}
