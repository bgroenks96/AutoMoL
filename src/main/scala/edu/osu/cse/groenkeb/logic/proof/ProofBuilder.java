package edu.osu.cse.groenkeb.logic.proof;

import java.util.Collection;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;

public final class ProofBuilder
{
  private final ProofContext context;
  
  public static ProofBuilder createFrom(Collection<Premise> premises)
  {
    return new ProofBuilder(new DefaultProofContext(premises));
  }
  
  public static ProofBuilder create()
  {
    return new ProofBuilder(new DefaultProofContext(ImmutableSet.of()));
  }
  
  public ProofContext getContext()
  {
    return context;
  }
  
  private ProofBuilder(ProofContext context)
  {
    this.context = context;
  }
  
  class A<K extends A<K, T>, T extends A<K, T>> {}
}
