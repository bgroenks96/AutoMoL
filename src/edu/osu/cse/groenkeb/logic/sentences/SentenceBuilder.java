package edu.osu.cse.groenkeb.logic.sentences;

import edu.osu.cse.groenkeb.logic.Proposition;
import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.operators.AndOperator;

public class SentenceBuilder
{
  public TermBuilder term(String propName)
  {
    return new TermBuilder(new Proposition(propName));
  }
  
  public Term prop(String propName)
  {
    return new Proposition(propName);
  }
  
  public class TermBuilder
  {
    private Term term;
    
    TermBuilder (Term term)
    {
      this.term = term;
    }
    
    public Term and(Term term)
    {
      return new BinaryOperatorSentence(this.term, term, new AndOperator());
    }
    
    public Term build()
    {
      return term;
    }
  }
}
