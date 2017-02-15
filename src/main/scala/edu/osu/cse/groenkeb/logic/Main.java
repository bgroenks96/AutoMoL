package edu.osu.cse.groenkeb.logic;

import edu.osu.cse.groenkeb.logic.parse.DefaultOperatorMatcher;
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer;
import edu.osu.cse.groenkeb.logic.parse.Notation;
import edu.osu.cse.groenkeb.logic.parse.SentenceParser;

public class Main
{
  public static void main(String[] args)
  {    
    final NodeRecursiveTokenizer tokenizer = new NodeRecursiveTokenizer();
    final SentenceParser parser = new SentenceParser(tokenizer, new DefaultOperatorMatcher());
    System.out.println(parser.parse("not ((C or D) and (A and B))", new Notation("infix")));
    System.out.println(parser.parse("and (not (or C D)) B", new Notation("prefix")));
    System.out.println(parser.parse("(A (C not) and)", new Notation("postfix")));
  }
}
