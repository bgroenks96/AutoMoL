package edu.osu.cse.groenkeb.logic.proof;

import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.proof.EmptyProof.NullProof;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Proof;

/**
 * Represents a premise that "stands proud" and has no former proof.
 */
public final class ProudPremise implements Premise
{
  private final Sentence sentence;
  
  public ProudPremise(Sentence sentence)
  {
    this.sentence = sentence;
  }
  
  @Override
  public Proof getProof()
  {
    return NullProof.get();
  }

  @Override
  public Sentence getSentence()
  {
    return sentence;
  }
}
