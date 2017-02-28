package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.rules.Discharge
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.rules.Vacuous
import edu.osu.cse.groenkeb.logic.proof.rules.Variate
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences

case class ProofContext private (val goal: Sentence,
                                 val premises: Seq[Premise],
                                 val assumptions: Seq[Assumption],
                                 val rules: RuleSet) {  
  def this(premises: Seq[Premise], rules: RuleSet) {
    this(Sentences.absurdity(), premises, List(), rules)
  }
  
  def withAssumption(assumption: Assumption) = ProofContext(goal, premises, assumptions:+assumption, rules)
  
  def withGoal(newGoal: Sentence) = ProofContext(newGoal, premises, assumptions, rules)
}

object ProofContext {
  def apply(premises: Seq[Premise], rules: RuleSet) = new ProofContext(premises, rules)
}
