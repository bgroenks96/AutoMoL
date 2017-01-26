package edu.osu.cse.groenkeb.logic;

public interface Sentence
{

  boolean matches(Sentence sentence);

  @Override
  String toString();
}
