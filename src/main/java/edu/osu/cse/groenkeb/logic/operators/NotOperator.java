package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.relation.Relation;
import edu.osu.cse.groenkeb.logic.relation.Relations;

public final class NotOperator implements UnaryOperator
{
  public Relation asRelation()
  {
    return Relations.not ();
  }
  
  @Override
  public String toString()
  {
    return "~";
  }
}
