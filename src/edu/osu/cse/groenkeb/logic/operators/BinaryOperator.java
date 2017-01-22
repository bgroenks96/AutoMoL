package edu.osu.cse.groenkeb.logic.operators;

import edu.osu.cse.groenkeb.logic.Operator;
import edu.osu.cse.groenkeb.logic.Term;

public interface BinaryOperator extends Operator
{
  boolean evaluate(Term a, Term b);
}
