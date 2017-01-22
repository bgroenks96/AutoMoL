package edu.osu.cse.groenkeb.logic.sentences;

import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.operators.BinaryOperator;

public class BinaryOperatorSentence implements Sentence
{
  private final BinaryOperator op;
  private final Term a, b;
  
  public BinaryOperatorSentence(Term a, Term b, BinaryOperator op)
  {
    this.a = a;
    this.b = b;
    this.op = op;
  }
  
  public BinaryOperator getOperator()
  {
    return this.op;
  }
  
  public Term getFirstTerm()
  {
    return this.a;
  }
  
  public Term getSecondTerm()
  {
    return this.b;
  }

  @Override
  public boolean evaluate ()
  {
    return op.evaluate (a, b);
  }
  
  @Override
  public String toString()
  {
    return String.format ("%s(%s,%s)", op, a, b);
  }
}
