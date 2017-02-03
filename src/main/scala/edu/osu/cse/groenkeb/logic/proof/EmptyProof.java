package edu.osu.cse.groenkeb.logic.proof;

import java.util.Optional;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.proof.interfaces.Assumption;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Proof;

public class EmptyProof implements Proof
{
  private Optional<Conclusion> conclusionMaybe;
  
  public EmptyProof(Conclusion conclusion)
  {
    this.conclusionMaybe = Optional.ofNullable(conclusion);
  }
  
  public EmptyProof()
  {
    this(null);
  }
  
  @Override
  public final ImmutableSet<Premise> getPremises()
  {
    return ImmutableSet.of();
  }
  
  @Override
  public final ImmutableList<Assumption> getAssumptions()
  {
    return ImmutableList.of();
  }

  @Override
  public final ImmutableList<Conclusion> getConclusions()
  {
    return conclusionMaybe.isPresent() ? ImmutableList.of(conclusionMaybe.get()) : ImmutableList.of();
  }
  
  public static final class NullProof extends EmptyProof
  {
    private static final NullProof instance = new NullProof();
    
    public static final NullProof get()
    {
      return instance;
    }
  }
}
