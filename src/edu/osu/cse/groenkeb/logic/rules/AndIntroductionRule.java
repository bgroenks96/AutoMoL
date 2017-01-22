package edu.osu.cse.groenkeb.logic.rules;

import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.TermSet;
import edu.osu.cse.groenkeb.logic.operators.AndOperator;
import edu.osu.cse.groenkeb.logic.sentences.BinaryOperatorSentence;

public final class AndIntroductionRule implements Rule
{
  @Override
  public TermSet apply (Term... terms)
  {
    assert terms.length == 2;
    return apply(terms[0], terms[1]);
  }
  
  private TermSet apply (Term a, Term b)
  {
    if (!a.evaluate () || !b.evaluate ())
    {
      return TermSet.EMPTY;
    }
    
    return new TermSet(new BinaryOperatorSentence(a, b, new AndOperator()));
  }
}
