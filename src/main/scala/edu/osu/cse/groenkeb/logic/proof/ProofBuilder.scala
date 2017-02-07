package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise
import edu.osu.cse.groenkeb.logic.proof.rules.Rule

class ProofBuilder(val context: ProofContext)
{
  def apply(rule: Rule)(major: Premise)(minor: Premise = EmptyPremise()) = {
    
  }
}