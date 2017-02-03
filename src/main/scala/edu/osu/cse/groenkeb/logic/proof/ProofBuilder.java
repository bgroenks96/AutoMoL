package edu.osu.cse.groenkeb.logic.proof;

import java.util.Collection;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.rules.Rule;

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
  
  public class OngoingConclusion
  {
    private final ProofBuilder proofBuilder;
    private final Rule rule;
    private Premise major, minor;
    private Conclusion result;
    
    private OngoingConclusion(ProofBuilder proofBuilder, Rule rule)
    {
      this.proofBuilder = proofBuilder;
      this.rule = rule;
    }
    
    public OngoingConclusion withMajor(Premise major)
    {
      assert minor == null;
      this.major = major;
      return this;
    }
    
    public ProofBuilder andMinor(Premise minor)
    {
      assert major != null;
      this.minor = minor;
      return proofBuilder;
    }
  }
}
