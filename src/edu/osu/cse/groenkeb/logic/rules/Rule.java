package edu.osu.cse.groenkeb.logic.rules;

import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.TermSet;

public interface Rule
{
  TermSet apply (Term...terms);
}
