package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.Term;

public class AndOperator implements BinaryOperator
{
  @Override
  public boolean evaluate (Term a, Term b)
  {
    return a.evaluate () && b.evaluate ();
  }
  
  @Override
  public String toString()
  {
    return "&";
  }
}
