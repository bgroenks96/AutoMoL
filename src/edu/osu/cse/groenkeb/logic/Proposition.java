package edu.osu.cse.groenkeb.logic;

public class Proposition implements Term
{
  private final String name;
  
  public Proposition(String name)
  {
    this.name = name;
  }
  
  public boolean evaluate()
  {
    return true;
  }
  
  public String getName()
  {
    return name;
  }
  
  @Override
  public boolean equals(Object obj)
  {
    return obj instanceof Proposition && name.equals (((Proposition)obj).name);
  }
  
  @Override
  public int hashCode()
  {
    return name.hashCode ();
  }
  
  @Override
  public String toString()
  {
    return getName();
  }
}
