package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.relation.Relation;
import edu.osu.cse.groenkeb.logic.relation.Relations;

public final class NotOperator implements UnaryOperator
{
  @Override
  public boolean matches(Operator operator)
  {
    return operator instanceof NotOperator;
  }
  
  @Override
  public Relation asRelation()
  {
    return Relations.not();
  }

  @Override
  public String toString()
  {
    return "~";
  }
}
