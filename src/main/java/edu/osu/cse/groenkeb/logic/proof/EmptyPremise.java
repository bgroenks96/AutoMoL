package edu.osu.cse.groenkeb.logic.proof;

import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.Sentences;
import edu.osu.cse.groenkeb.logic.proof.EmptyProof.NullProof;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Proof;

public final class EmptyPremise implements Premise
{
  public static EmptyPremise get()
  {
    return new EmptyPremise();
  }
  
  @Override
  public Sentence getSentence()
  {
    return Sentences.nil();
  }

  @Override
  public Proof getProof()
  {
    return NullProof.get();
  }
  
  private EmptyPremise();
}
