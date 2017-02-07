package edu.osu.cse.groenkeb.logic.proof;

import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableSet;

import edu.osu.cse.groenkeb.logic.MetaRelation;
import edu.osu.cse.groenkeb.logic.SentenceRelation;
import edu.osu.cse.groenkeb.logic.TruthRelation;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Assumption;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;

public abstract class ProofContext
{
  private final ImmutableSet<Premise> initialPremises;
  
  private final ImmutableSet<MetaRelation> relations;

  protected ProofContext(Collection<Premise> premises)
  {
    this(ImmutableSet.copyOf(premises), ImmutableSet.of());
  }

  protected ProofContext(Premise... premises)
  {
    this(Arrays.asList(premises));
  }

  protected abstract ProofContext createFrom(ImmutableSet<Premise> initialPremises,
                                             ImmutableSet<MetaRelation> relations);
  
  public Set<MetaRelation> relationsFor(Assumption assumption)
  {
    final SentenceRelation object = assumption.getSentence().relate();
    return relations.stream().filter(r -> r.contains(object)).collect(Collectors.toSet());
  }
  
  public final ProofContext withAssumption(Assumption assumption)
  {
    final SentenceRelation relation = new SentenceRelation(assumption.getSentence());
    ImmutableSet.builder().addAll(relations).add(new TruthRelation(relation));
    return createFrom(initialPremises, relations);
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
                         ImmutableSet<MetaRelation> relations)
  {
    this.initialPremises = initialPremises;
    this.relations = relations;
  }
}
