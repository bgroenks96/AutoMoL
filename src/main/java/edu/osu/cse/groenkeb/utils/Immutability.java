package edu.osu.cse.groenkeb.utils;

import java.util.Set;

import com.google.common.collect.Sets;
import com.google.common.collect.Sets.SetView;

public final class Immutability
{
  public static <T> SetView<T> viewFor(Set<T> set)
  {
    return Sets.intersection(set, set);
  }
  
  private Immutability()
  {
  }
}
