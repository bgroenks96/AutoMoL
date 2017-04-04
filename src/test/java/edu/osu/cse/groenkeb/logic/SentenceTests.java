package edu.osu.cse.groenkeb.logic;

import static edu.osu.cse.groenkeb.logic.Sentences.and;
import static edu.osu.cse.groenkeb.logic.Sentences.atom;
import static edu.osu.cse.groenkeb.logic.Sentences.not;
import static edu.osu.cse.groenkeb.logic.Sentences.or;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class SentenceTests
{
  @Test
  public void testAtomicSentenceToString()
  {
    assertEquals("A", atom("A").toString());
  }
  
  @Test
  public void testAtomicPredcateToString()
  {
    assertEquals("A[b.c]", atom("A[b,c]").toString());
  }
  
  @Test
  public void testBinarySentenceToString()
  {
    assertEquals("and(A,B)", and(atom("A"), atom("B")).toString());
  }
  
  @Test
  public void testUnarySentenceToString()
  {
    assertEquals("not(A)", not(atom("A")).toString());
  }
  
  @Test
  public void testAtomicSentenceMatch()
  {
    assertTrue(atom("A").matches(atom("A")));
  }
  
  @Test
  public void testAtomicSentenceNoMatch()
  {
    assertFalse(atom("A").matches(atom("B")));
  }
  
  @Test
  public void testBinarySentenceMatch()
  {
    assertTrue(and(atom("A"), atom("B")).matches(and(atom("A"), atom("B"))));
  }
  
  @Test
  public void testBinarySentenceNoMatch()
  {
    assertFalse(and(atom("A"), atom("B")).matches(and(atom("A"), atom("C"))));
  }
  
  @Test
  public void testUnarySentenceMatch()
  {
    assertTrue(not(atom("A")).matches(not(atom("A"))));
  }
  
  @Test
  public void testUnarySentenceNoMatch()
  {
    assertFalse(not(atom("A")).matches(not(atom("B"))));
  }
  
  @Test
  public void testComplexSentenceMatch()
  {
    final Sentence sentence = and(not(or(atom("C"), atom("D"))), atom("B"));
    assertTrue(sentence.matches(sentence));
  }
  
  @Test
  public void testSentenceSubstitution()
  {
    final Sentence sentence = and(not(atom("C")), atom("L[x]"));
    final Sentence expected = and(not(atom("C")), atom("L[a]"));
    final Sentence substitute = sentence.substitute (new Term("x"), new Term("a"));
    assertTrue(substitute.matches (expected));
  }
  
  @Test
  public void testSimpleSentenceContainsAtom()
  {
    final Sentence sentenceA = atom("A");
    final Sentence sentenceAB = and(atom("A"), atom("B"));
    assertTrue(sentenceAB.contains(sentenceA));
  }
  
  @Test
  public void testComplexSentenceContainsAtom()
  {
    final Sentence sentenceA = atom("A");
    final Sentence complexSentence = or(and(not(atom("A")), atom("B")), atom("C"));
    assertTrue(complexSentence.contains(sentenceA));
  }
  
  @Test
  public void testComplexSentenceContainsBinarySentence()
  {
    final Sentence sentenceBD = and(atom("B"), atom("D"));
    final Sentence complexSentence = or(and(not(atom("A")), sentenceBD), atom("C"));
    assertTrue(complexSentence.contains(sentenceBD));
  }
}
