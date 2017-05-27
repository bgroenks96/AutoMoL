package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.rules.Discharge
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.rules.Vacuous
import edu.osu.cse.groenkeb.logic.proof.rules.Variate
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.rules.Rule

case class ProofContext(val goal: Sentence,
                        val premises: Seq[Premise],
                        val rules: RuleSet) {  
  def withAssumptions(newAssumptions: Assumption*) = ProofContext(goal, premises ++ newAssumptions, rules)
  
  def withRuleSet(newRules: RuleSet) = ProofContext(goal, premises, newRules)
  
  def withGoal(newGoal: Sentence) = ProofContext(newGoal, premises, rules)
  
  def lessAssumptions(assumptions: Assumption*) = ProofContext(goal,
                                                               premises.filter { p => !assumptions.exists { a => a.sentence.matches(p.sentence) } },
                                                               rules)
}
