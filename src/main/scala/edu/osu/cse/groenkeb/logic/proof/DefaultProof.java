package edu.osu.cse.groenkeb.logic.proof;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Proof;

public final class DefaultProof implements Proof
{
  private final Conclusion conclusion;
  private final ImmutableSet<Premise> initialPremises;
  
  public DefaultProof(Conclusion conclusion, ImmutableSet<Premise> initialPremises)
  {
    this.conclusion = conclusion;
    this.initialPremises = initialPremises;
  }
  
  @Override
  public Conclusion getConclusion()
  {
    return conclusion;
  }
  
  @Override
  public ImmutableSet<Premise> getInitialPremises()
  {
    return initialPremises;
  }
}
