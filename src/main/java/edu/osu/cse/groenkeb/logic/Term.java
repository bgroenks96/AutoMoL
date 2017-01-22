package edu.osu.cse.groenkeb.logic;

public interface Term
{
  String getName();
  
  void visit(TermVisitor visitor);
}
