package edu.osu.cse.groenkeb.logic;

import edu.osu.cse.groenkeb.logic.operators.UnaryOperator;
import edu.osu.cse.groenkeb.utils.Types;

public final class UnarySentence implements Sentence
{
  private final Sentence sentence;
  private final UnaryOperator unaryOperator;

  public UnarySentence(final Sentence sentence, final UnaryOperator unaryOperator)
  {
    assert sentence != null;
    assert unaryOperator != null;

    this.sentence = sentence;
    this.unaryOperator = unaryOperator;
  }

  public Sentence getSentence()
  {
    return sentence;
  }

  public UnaryOperator getOperator()
  {
    return unaryOperator;
  }

  @Override
  public boolean matches(Sentence sentence)
  {
    return Types.safeMatch(getClass(), sentence,
                           s -> sentence.matches(s.getSentence()) && unaryOperator.matches(s.getOperator()));
  }

  @Override
  public String toString()
  {
    return String.format("%s(%s)", unaryOperator, sentence);
  }
}
