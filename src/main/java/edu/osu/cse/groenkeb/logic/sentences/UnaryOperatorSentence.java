package edu.osu.cse.groenkeb.logic.sentences;

import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.TermVisitor;
import edu.osu.cse.groenkeb.logic.operators.Operator;
import edu.osu.cse.groenkeb.logic.operators.UnaryOperator;

public class UnaryOperatorSentence implements Sentence<UnaryOperator>
{
  private final Term term;
  private final UnaryOperator operator;
  
  public UnaryOperatorSentence(Term term, UnaryOperator operator)
  {
    this.term = term;
    this.operator = operator;
  }
  
  public Term getTerm()
  {
    return term;
  }

  @Override
  public String getName ()
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public void visit (TermVisitor visitor)
  {
    visitor
  }

  @Override
  public Operator getOperator ()
  {
    // TODO Auto-generated method stub
    return null;
  }
}
