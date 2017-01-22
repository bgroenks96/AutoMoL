package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.Term;

public final class NotOperator implements UnaryOperator
{
  @Override
  public boolean evaluate (Term a)
  {
    return !a.evaluate ();
  }
}
