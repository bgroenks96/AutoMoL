package edu.osu.cse.groenkeb.logic.proof.types

class ProofContext private (val premises: Seq[Premise], val assumptions: Seq[Assumption]) {  
  def this(premises: Seq[Premise]) {
    this(premises, List())
  }
}
