package edu.osu.cse.groenkeb.logic;

import edu.osu.cse.groenkeb.logic.operators.AndOperator;
import edu.osu.cse.groenkeb.logic.operators.NotOperator;

public final class Sentences
{
  public static Sentence atom(String name)
  {
    return new AtomicSentence(new Atom(name));
  }
  
  public static Sentence and(Sentence left, Sentence right)
  {
    final AndOperator operator = new AndOperator();
    return new BinarySentence(left, right, operator);
  }
  
  public static Sentence not(Sentence operand)
  {
    final NotOperator operator = new NotOperator();
    return new UnarySentence(operand, operator);
  }
  
  public static Sentence nil()
  {
    return NullSentence.get();
  }
  
//  private Sentence result;
//  
//  public SentenceBuilder atom(String name)
//  {
//    result = new AtomicSentence(new Atom(name));
//    return this;
//  }
//  
//  public SentenceBuilder newAtom(String name)
//  {
//    return new SentenceBuilder().atom(name);
//  }
//  
//  public SentenceBuilder and(Sentence sentence)
//  {
//    result = new BinarySentence(result, sentence, new AndOperator());
//    return this;
//  }
//  
//  public SentenceBuilder not()
//  {
//    result = new UnarySentence(result, new NotOperator());
//    return this;
//  }
//  
//  public Sentence build()
//  {
//    if (result == null)
//    {
//      throw new IllegalStateException("You didn't build anything!");
//    }
//    
//    return result;
//  }
}
