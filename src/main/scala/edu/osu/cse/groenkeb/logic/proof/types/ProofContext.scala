package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

case class ProofContext private (val premises: Seq[Premise], val assumptions: Seq[Assumption], val rules: RuleSet) {  
  def this(premises: Seq[Premise], rules: RuleSet) {
    this(premises, List(), rules)
  }
}
