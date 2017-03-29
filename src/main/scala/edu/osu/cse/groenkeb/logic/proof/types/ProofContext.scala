package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.rules.Discharge
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.rules.Vacuous
import edu.osu.cse.groenkeb.logic.proof.rules.Variate
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.rules.Rule

case class ProofContext private (val goal: Sentence,
                                 val premises: Seq[Premise],
                                 val assumptions: Seq[Assumption],
                                 val rules: RuleSet) {  
  def this(goal: Sentence, premises: Seq[Premise], rules: RuleSet) {
    this(goal, premises, List(), rules)
  }
  
  def withAssumption(assumption: Assumption) = ProofContext(goal, premises, assumptions:+assumption, rules)
  
  def withRuleSet(newRules: RuleSet) = ProofContext(goal, premises, assumptions, newRules)
  
  def withGoal(newGoal: Sentence) = ProofContext(newGoal, premises, assumptions, rules)
  
  def lessAssumptions(assumptions: Assumption*) = ProofContext(goal,
                                                              premises,
                                                              assumptions.diff(assumptions),
                                                              rules)
}

object ProofContext {
  def apply(goal: Sentence, premises: Seq[Premise], rules: RuleSet) = new ProofContext(goal, premises, rules)
}
