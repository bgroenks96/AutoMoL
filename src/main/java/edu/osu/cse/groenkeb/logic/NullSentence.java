package edu.osu.cse.groenkeb.logic;

public class NullSentence implements Sentence
{
  private static final NullSentence instance = new NullSentence();
  
  public static NullSentence get()
  {
    return instance;
  }
  
  @Override
  public boolean matches(Sentence sentence)
  {
    return instance.equals(sentence);
  }
  
  @Override
  public String toString()
  {
    return "";
  }
  
  private NullSentence()
  {
  }
}
