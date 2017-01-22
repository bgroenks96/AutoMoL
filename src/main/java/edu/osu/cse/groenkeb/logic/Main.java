package edu.osu.cse.groenkeb.logic;

import edu.osu.cse.groenkeb.logic.rules.AndIntroductionRule;
import edu.osu.cse.groenkeb.logic.rules.Rule;
import edu.osu.cse.groenkeb.logic.sentences.SentenceBuilder;

public class Main
{
  public static void main(String[] args)
  {
    final SentenceBuilder builder = new SentenceBuilder();
    final Term termAB = builder.term ("A").and (builder.prop ("B"));
    final Term propC = builder.prop ("C");
    final Rule andIntro = new AndIntroductionRule();
    System.out.println (termAB + "   " + propC);
    System.out.println ("___________");
    System.out.println (andIntro.apply (termAB, propC));
  }
}
