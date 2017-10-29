package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

object ProofContext {
  def apply(goal: Sentence,
            rules: RuleSet,
            premises: Seq[Premise]) = new ProofContext(goal, rules, premises.toSet)
  
  def apply(goal: Sentence, premises: Seq[Premise] = Nil)(implicit rules: RuleSet) = new ProofContext(goal, rules, premises.toSet)
  
  private def apply(goal: Sentence,
                    rules: RuleSet,
                    premises: Seq[Premise],
                    discharges: Seq[Assumption]) = new ProofContext(goal, rules, premises.toSet, discharges)
}

case class ProofContext private (val goal: Sentence,
                                 val rules: RuleSet,
                                 val premises: Set[Premise],
                                 val discharges: Seq[Assumption]) {
  def this(goal: Sentence, rules: RuleSet, premises: Set[Premise]) = this(goal, rules, premises, Nil)
  
  def withAssumptions(newAssumptions: Assumption*) = ProofContext(goal, rules, premises ++ newAssumptions, discharges)
  
  def withRuleSet(newRules: RuleSet) = ProofContext(goal, newRules, premises, discharges)
  
  def withGoal(newGoal: Sentence) = ProofContext(newGoal, rules, premises, discharges)
  
  def withDischarged(assumptions: Assumption*) = {
    require(assumptions.toSet subsetOf premises)
    ProofContext(goal,
                 rules,
                 premises filterNot { assumptions.toSet },
                 discharges ++ assumptions)           
  }
                                                               
  def withDischarge(discharged: Assumption) = {
    require(premises.contains(discharged))
    ProofContext(goal, rules, premises, discharges :+ discharged)
  }
  
  def lessPremises(restrict: Premise*) = ProofContext(goal, rules, premises.filterNot { p => restrict.exists { r => r.matches(p) } }, discharges)
  
  def has(premise: Premise) = premises.exists { p => p.matches(premise) }
  
  def hasGoal(sentence: Sentence) = goal.matches(sentence)
}
