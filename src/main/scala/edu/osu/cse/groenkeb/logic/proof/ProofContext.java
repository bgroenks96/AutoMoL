package edu.osu.cse.groenkeb.logic.proof;

import java.util.Arrays;
import java.util.Collection;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.ObjectRelation;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Assumption;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;

public abstract class ProofContext
{
  private final ImmutableSet<Premise> initialPremises;
  
  private final ImmutableSet<ObjectRelation> relations;

  protected ProofContext(Collection<Premise> premises)
  {
    this(ImmutableSet.copyOf(premises), ImmutableSet.of());
  }

  protected ProofContext(Premise... premises)
  {
    this(Arrays.asList(premises));
  }

  protected abstract ProofContext createFrom(ImmutableSet<Premise> initialPremises,
                                             ImmutableSet<ObjectRelation> relations);
  
  public final ProofContext withAssumption(Assumption assumption)
  {
    return null;
  }
  
  public final ProofContext withDischarge(Conclusion conclusion, Assumption...assumptions)
  {
    return null;
  }

  public final ProofContext copy()
  {
    return createFrom(initialPremises, relations);
  }

  @Override
  public final Object clone()
  {
    return copy();
  }

  protected ProofContext(ImmutableSet<Premise> initialPremises,
                         ImmutableSet<ObjectRelation> relations)
  {
    this.initialPremises = initialPremises;
    this.relations = relations;
  }
}
