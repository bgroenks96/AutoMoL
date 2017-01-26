package edu.osu.cse.groenkeb.logic.proof.interfaces;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

public interface Proof
{
  /**
   * @return the set of all premises given at the start of the proof
   */
  ImmutableSet<Premise> getPremises();
  
  /**
   * @return a list of conclusions in the order they appear in the proof from top to bottom, left to right.
   */
  ImmutableList<Conclusion> getConclusions();
}
