package edu.osu.cse.groenkeb.logic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class TermSet implements Iterable<Term>
{
  public static final TermSet EMPTY = new TermSet();
  
  private final List<Term> terms = new ArrayList<Term>();
  
  public TermSet(final Term...terms)
  {
    this.terms.addAll (Arrays.asList (terms));
  }
  
  @Override
  public Iterator<Term> iterator()
  {
    return terms.iterator ();
  }
  
  public int count()
  {
    return terms.size ();
  }
  
  @Override
  public String toString()
  {
    return Arrays.toString (terms.toArray ());
  }
}
