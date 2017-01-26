package edu.osu.cse.groenkeb.logic;

import edu.osu.cse.groenkeb.logic.operators.BinaryOperator;
import edu.osu.cse.groenkeb.utils.Types;

public final class BinarySentence implements Sentence
{
  private final Sentence leftSentence, rightSentence;
  private final BinaryOperator binaryOperator;

  public BinarySentence(Sentence leftSentence, Sentence rightSentence, BinaryOperator binaryOperator)
  {
    assert leftSentence != null;
    assert rightSentence != null;
    assert binaryOperator != null;

    this.leftSentence = leftSentence;
    this.rightSentence = rightSentence;
    this.binaryOperator = binaryOperator;
  }

  public Sentence getLeftSentence()
  {
    return leftSentence;
  }

  public Sentence getRightSentence()
  {
    return rightSentence;
  }

  public BinaryOperator getOperator()
  {
    return binaryOperator;
  }

  @Override
  public boolean matches(Sentence sentence)
  {
    return Types.safeMatch(getClass(), sentence, s -> leftSentence.matches(s.getLeftSentence())
                                                      && rightSentence.matches(s.getRightSentence()));
  }

  @Override
  public String toString()
  {
    return String.format("%s(%s,%s)", binaryOperator, leftSentence, rightSentence);
  }
}
