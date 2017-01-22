package edu.osu.cse.groenkeb.logic.rules;

import java.util.function.BinaryOperator;

import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.TermSet;
import edu.osu.cse.groenkeb.logic.sentences.BinaryOperatorSentence;

public class AndEliminationRule implements Rule
{
  public TermSet apply(Term...terms)
  {
    assert terms.length == 1;
    Term term = terms[0];
    if (term instanceof BinaryOperatorSentence)
    {
      return apply((BinaryOperatorSentence)term);
    }
    
    return TermSet.EMPTY;
  }
  
  private TermSet apply(BinaryOperatorSentence term)
  {
    if (term.getOperator () instanceof BinaryOperator)
    {
      return new TermSet(term.getFirstTerm (), term.getSecondTerm());
    }
    
    return TermSet.EMPTY;
  }
}
