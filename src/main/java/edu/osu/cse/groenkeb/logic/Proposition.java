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
  
  @Override
  public String getName()
  {
    return name;
  }
  
  @Override
  public String toString()
  {
    return getName();
  }

  @Override
  public void visit (TermVisitor visitor)
  {
    visitor.prop (this);
  }
}
