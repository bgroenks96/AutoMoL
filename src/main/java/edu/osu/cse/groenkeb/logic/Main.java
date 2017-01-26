package edu.osu.cse.groenkeb.logic;

import static edu.osu.cse.groenkeb.logic.Sentences.and;
import static edu.osu.cse.groenkeb.logic.Sentences.atom;
import static edu.osu.cse.groenkeb.logic.Sentences.not;

public class Main
{
  public static void main(String[] args)
  {
    final Sentence sentence = and(atom("A"), not(atom("B")));
    System.out.println(sentence);
  }
}
