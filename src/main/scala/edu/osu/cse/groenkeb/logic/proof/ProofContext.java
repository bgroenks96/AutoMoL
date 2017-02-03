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

public abstract class ProofContext
{
  private final Set<Premise> initialPremises = Sets.newHashSet();

  private final Set<Assumption> hangingAssumptions = Sets.newHashSet();

  private final BiMap<Assumption, Conclusion> discharges = HashBiMap.create();

  protected ProofContext(Collection<Premise> premises)
  {
    this(premises, ImmutableSet.of(), ImmutableMap.of());
  }

  protected ProofContext(Premise... premises)
  {
    this(Arrays.asList(premises));
  }

  protected abstract ProofContext createFrom(Set<Premise> initialPremises,
                                             Set<Assumption> availableAssumptions,
                                             Map<Assumption, Conclusion> discharges);

  public SetView<Assumption> getAvailableAssumptions()
  {
    return Sets.union(Sets.union(hangingAssumptions, discharges.values()), initialPremises);
  }

  public final SetView<Assumption> getDischargedAssumptions()
  {
    return Immutability.viewFor(discharges.keySet());
  }

  public final Conclusion conclusionFor(Assumption dischargedAssumption)
  {
    assert discharges.containsKey(dischargedAssumption);
    return discharges.get(dischargedAssumption);
  }

  public final Assumption assumptionFor(Conclusion conclusion)
  {
    assert discharges.inverse().containsKey(conclusion);
    return discharges.inverse().get(conclusion);
  }

  public final ProofContext copy()
  {
    return createFrom(initialPremises, hangingAssumptions, discharges);
  }

  @Override
  public final Object clone()
  {
    return copy();
  }

  protected final void assume(Assumption assumption)
  {
    hangingAssumptions.add(assumption);
  }

  protected final void discharge(Assumption assumption, Conclusion conclusion)
  {
    final boolean dischargable = hangingAssumptions.contains(assumption);
    assert dischargable;
    if (!dischargable) return;
    hangingAssumptions.remove(assumption);
    discharges.put(assumption, conclusion);
  }

  protected ProofContext(Collection<Premise> premises,
                         Collection<Assumption> hangingAssumptions,
                         Map<Assumption, Conclusion> discharges)
  {
    this.initialPremises.addAll(premises);
    this.hangingAssumptions.addAll(hangingAssumptions);
    this.discharges.putAll(discharges);
  }
}
