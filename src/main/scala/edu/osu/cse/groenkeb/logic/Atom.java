package edu.osu.cse.groenkeb.logic;

public final class Atom
{
  public static final Atom ABSURDITY = new Atom("!");
  
  private final String name;

  public Atom(String name)
  {
    this.name = name;
  }

  public boolean matches(Atom atom)
  {
    assert name != null;
    return this.name.equals(atom.name);
  }

  @Override
  public String toString()
  {
    return name;
  }
}
