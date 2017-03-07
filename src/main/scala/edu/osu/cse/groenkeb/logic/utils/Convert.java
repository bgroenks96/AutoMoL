package edu.osu.cse.groenkeb.logic.utils;

import java.util.ArrayList;
import java.util.Collection;

import scala.collection.JavaConversions;
import scala.collection.Seq;

public final class Convert
{
  public static final <T> Seq<T> toScalaSeq(final Iterable<T> itr)
  {
    return JavaConversions.asScalaIterator(itr.iterator()).toSeq();
  }
  
  public static final <T> Collection<T> toJavaCollection(final scala.collection.Iterable<T> itr)
  {
    return JavaConversions.asJavaCollection(itr);
  }
  
  public static final <T> Seq<T> emptyScalaSeq()
  {
    return JavaConversions.asScalaBuffer(new ArrayList<T>());
  }
  
  private Convert() {}
}
