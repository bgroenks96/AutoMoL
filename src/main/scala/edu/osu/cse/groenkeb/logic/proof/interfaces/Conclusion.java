package edu.osu.cse.groenkeb.logic.proof.interfaces;

import java.util.Optional;

public interface Conclusion extends Premise
{
  Premise getMajor();
  
  Optional<Premise> getMinor();
  
  boolean hasMinor();
}
