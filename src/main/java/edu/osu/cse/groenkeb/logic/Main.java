package edu.osu.cse.groenkeb.logic;

import static edu.osu.cse.groenkeb.logic.Sentences.*;

public class Main
{
  public static void main(String[] args)
  {
    final Sentence sentence = and(atom("A"), not(atom("B")));
    System.out.println(sentence);
  }
}
