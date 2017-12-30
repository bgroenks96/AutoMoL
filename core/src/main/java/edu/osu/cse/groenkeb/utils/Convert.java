package edu.osu.cse.groenkeb.utils;

import java.util.Collections;

import scala.collection.JavaConversions;

public class Convert
{
  public static <T> java.util.List<T> toJavaList(scala.collection.Seq<T> seq)
  {
    return JavaConversions.seqAsJavaList(seq);
  }
  
  public static <T> scala.collection.Seq<T> toScalaSeq(java.lang.Iterable <T> itr)
  {
    return JavaConversions.iterableAsScalaIterable (itr).toSeq ();
  }
  
  public static <T> scala.collection.immutable.Set<T> toScalaSet(java.util.Set <T> set)
  {
    return JavaConversions.asScalaSet (set).toSet ();
  }
  
  public static <T> scala.collection.immutable.Seq<T> emptyScalaSeq()
  {
    return new scala.collection.immutable.VectorBuilder ().result();
  }
  
  public static <T> scala.collection.immutable.Set<T> emptyScalaSet()
  {
    return new scala.collection.immutable.HashSet<> ();
  }
  
  public static <T> scala.collection.Iterable<T> toScalaItr(java.lang.Iterable <T> itr)
  {
    return JavaConversions.iterableAsScalaIterable(itr);
  }
}
