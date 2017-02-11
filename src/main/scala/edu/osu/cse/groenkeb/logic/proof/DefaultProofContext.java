package edu.osu.cse.groenkeb.logic.proof;

import java.util.Collection;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.ObjectRelation;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;

final class DefaultProofContext extends ProofContext
{
  DefaultProofContext(Collection<Premise> initialPremises)
  {
    super(initialPremises);
  }
  
  @Override
  protected ProofContext createFrom(ImmutableSet<Premise> initialPremises,
                                    ImmutableSet<ObjectRelation> relations)
  {
    return new DefaultProofContext(initialPremises, relations);
  }
  
  private DefaultProofContext(ImmutableSet<Premise> initialPremises,
                              ImmutableSet<ObjectRelation> relations)
  {
    super(initialPremises, relations);
  }
}
