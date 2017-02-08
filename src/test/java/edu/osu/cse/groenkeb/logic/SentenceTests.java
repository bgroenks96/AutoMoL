package edu.osu.cse.groenkeb.logic;

import static org.junit.Assert.*;

import static edu.osu.cse.groenkeb.logic.Sentences.*;

import org.junit.Test;

public class SentenceTests
{
  @Test
  public void testAtomicSentenceToString()
  {
    assertEquals("A", atom("A").toString());
  }
  
  @Test
  public void testBinarySentenceToString()
  {
    assertEquals("and(A, B)", and(atom("A"), atom("B")).toString());
  }
  
  @Test
  public void testUnarySentenceToString()
  {
    assertEquals("~A", not(atom("A")).toString());
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
}
