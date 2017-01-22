package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.relation.Relation;
import edu.osu.cse.groenkeb.logic.relation.Relations;

public class AndOperator implements BinaryOperator
{
  @Override
  public Relation asRelation()
  {
    return Relations.and ();
  }
  
  @Override
  public String toString()
  {
    return "&";
  }
}
