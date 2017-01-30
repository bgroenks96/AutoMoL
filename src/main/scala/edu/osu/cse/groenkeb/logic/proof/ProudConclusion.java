package edu.osu.cse.groenkeb.logic.proof;

import java.util.Optional;

import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.proof.EmptyProof.NullProof;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Proof;

/**
 * Represents a premise/conclusion that "stands proud" and has no former proof.
 */
public final class ProudConclusion implements Conclusion
{
  private final Sentence sentence;
  
  public ProudConclusion(Sentence sentence)
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

  @Override
  public Premise getMajor()
  {
    return EmptyPremise.get();
  }

  @Override
  public Optional<Premise> getMinor()
  {
    return Optional.empty();
  }

  @Override
  public boolean hasMinor()
  {
    return false;
  }
}
