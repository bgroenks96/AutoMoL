package edu.osu.cse.groenkeb.utils;

import java.util.function.Predicate;

public final class Types
{
  public static <T> boolean safeMatch(Class<T> type, Object obj, Predicate<T> matchPredicate)
  {
    if (!type.isInstance(obj)) return false;
    return matchPredicate.test(type.cast(obj));
  }

  private Types()
  {}
}
