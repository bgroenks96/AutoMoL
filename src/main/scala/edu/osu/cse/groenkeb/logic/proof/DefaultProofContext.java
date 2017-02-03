package edu.osu.cse.groenkeb.logic.proof;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

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
  protected ProofContext createFrom(Set<Premise> initialPremises,
                                    Set<Assumption> availableAssumptions,
                                    Map<Assumption, Conclusion> discharges)
  {
    return new DefaultProofContext(initialPremises, availableAssumptions, discharges);
  }
  
  private DefaultProofContext(Set<Premise> initialPremises, Set<Assumption> hangingAssumptions, Map<Assumption, Conclusion> discharges)
  {
    super(initialPremises, hangingAssumptions, discharges);
  }
}
