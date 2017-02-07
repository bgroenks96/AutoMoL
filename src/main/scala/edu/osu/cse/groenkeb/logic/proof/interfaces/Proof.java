package edu.osu.cse.groenkeb.logic.proof.interfaces;

import com.google.common.collect.ImmutableSet;

public interface Proof
{
  /**
   * @return the set of all premises given at the start of the proof
   */
  ImmutableSet<Premise> getInitialPremises();
  
  /**
   * @return the terminal conclusion of the proof
   */
  Conclusion getConclusion();
}
