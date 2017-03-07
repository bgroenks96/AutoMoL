package edu.osu.cse.groenkeb.logic;

public final class Sentences
{
  public static Sentence atom(String str)
  {
    return new AtomicSentence(Atom.parse(str));
  }
  
  public static Sentence and(Sentence left, Sentence right)
  {
    final And operator = new And();
    return new BinarySentence(left, right, operator);
  }
  
  public static Sentence or(Sentence left, Sentence right)
  {
    final Or operator = new Or();
    return new BinarySentence(left, right, operator);
  }
  
  public static Sentence not(Sentence operand)
  {
    final Not operator = new Not();
    return new UnarySentence(operand, operator);
  }
  
  public static Sentence absurdity()
  {
    return new AtomicSentence(Atom.absurdity());
  }
  
  public static Sentence nil()
  {
    return new NullSentence();
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
