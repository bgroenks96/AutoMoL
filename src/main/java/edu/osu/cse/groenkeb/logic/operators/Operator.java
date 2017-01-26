package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.relation.Relation;

public interface Operator
{
  Relation asRelation();

  boolean matches(Operator operator);
}
