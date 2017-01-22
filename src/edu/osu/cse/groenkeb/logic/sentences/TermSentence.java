package edu.osu.cse.groenkeb.logic.sentences;

import edu.osu.cse.groenkeb.logic.Term;

public final class TermSentence implements Sentence
{
  private final Term term;
  
  public TermSentence(Term term)
  {
    this.term = term;
  }
  
  public Term getTerm ()
  {
    return term;
  }

  @Override
  public boolean evaluate ()
  {
    return term.evaluate ();
  }
}
