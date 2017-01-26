package edu.osu.cse.groenkeb.logic;

import edu.osu.cse.groenkeb.utils.Types;

public final class AtomicSentence implements Sentence
{
  private final Atom atom;

  public AtomicSentence(Atom atom)
  {
    assert atom != null;
    this.atom = atom;
  }

  public Atom getAtom()
  {
    return atom;
  }

  @Override
  public boolean matches(Sentence sentence)
  {
    return Types.safeMatch(getClass(), sentence, s -> atom.matches(s.getAtom()));
  }

  @Override
  public String toString()
  {
    return atom.toString();
  }
}
