package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.rules.Discharge
import edu.osu.cse.groenkeb.logic.proof.rules.Required
import edu.osu.cse.groenkeb.logic.proof.rules.Vacuous
import edu.osu.cse.groenkeb.logic.proof.rules.Variate

case class ProofContext private (val premises: Seq[Premise],
                                 val assumptions: Seq[Assumption],
                                 val rules: RuleSet) {  
  def this(premises: Seq[Premise], rules: RuleSet) {
    this(premises, List(), rules)
  }
  
  def withAssumption(assumption: Assumption) = ProofContext(premises, assumptions:+assumption, rules)
}
