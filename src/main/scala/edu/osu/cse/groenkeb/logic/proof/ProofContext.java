package edu.osu.cse.groenkeb.logic.proof;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import com.google.common.collect.Sets.SetView;

import edu.osu.cse.groenkeb.logic.proof.interfaces.Assumption;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.utils.Immutability;

abstract class ProofContext
{
  private final Set<Premise> initialPremises = Sets.newHashSet();

  private final Set<Assumption> availableAssumptions = Sets.newHashSet();

  private final BiMap<Assumption, Conclusion> discharges = HashBiMap.create();

  ProofContext(Collection<Premise> premises)
  {
    this(premises, ImmutableSet.of(), ImmutableMap.of());
  }

  ProofContext(Premise... premises)
  {
    this(Arrays.asList(premises));
  }

  protected abstract ProofContext createFrom(Set<Premise> initialPremises,
                                             Set<Assumption> availableAssumptions,
                                             Map<Assumption, Conclusion> discharges);

  public final SetView<Assumption> getAvailableAssumptions()
  {
    return Immutability.viewFor(Sets.union(availableAssumptions, initialPremises));
  }

  public final SetView<Assumption> getDischargedAssumptions()
  {
    return Immutability.viewFor(discharges.keySet());
  }

  public final Conclusion conclusionFor(Assumption dischargedAssumption)
  {
    return discharges.get(dischargedAssumption);
  }

  public final Assumption assumptionFor(Conclusion conclusion)
  {
    return discharges.inverse().get(conclusion);
  }

  public final ProofContext copy()
  {
    return createFrom(initialPremises, availableAssumptions, discharges);
  }

  @Override
  public final Object clone()
  {
    return copy();
  }
  
  protected final void assume(Assumption assumption)
  {
    availableAssumptions.add(assumption);
  }
  
  protected final void discharge(Assumption assumption, Conclusion conclusion)
  {
    final boolean dischargable = availableAssumptions.contains(assumption);
    assert dischargable;
    if (!dischargable) return;
    availableAssumptions.remove(assumption);
    discharges.put(assumption, conclusion);
  }

  private ProofContext(Collection<Premise> premises,
                       Collection<Assumption> available,
                       Map<Assumption, Conclusion> discharges)
  {
    this.initialPremises.addAll(premises);
    this.availableAssumptions.addAll(available);
    this.discharges.putAll(discharges);
  }
}
