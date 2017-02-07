package edu.osu.cse.groenkeb.logic.proof;

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.MetaRelation;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Assumption;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;

final class DefaultProofContext extends ProofContext
{
  DefaultProofContext(Collection<Premise> initialPremises)
  {
    super(initialPremises);
  }
  
  @Override
  protected ProofContext createFrom(ImmutableSet<Premise> initialPremises,
                                    ImmutableSet<MetaRelation> relations)
  {
    return new DefaultProofContext(initialPremises, relations);
  }
  
  private DefaultProofContext(ImmutableSet<Premise> initialPremises,
                              ImmutableSet<MetaRelation> relations)
  {
    super(initialPremises, relations);
  }
}
