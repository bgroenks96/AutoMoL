package edu.osu.cse.groenkeb.logic.sentences;

import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.operators.Operator;

public interface Sentence<T extends Operator> extends Term
{
  T getOperator();
}
