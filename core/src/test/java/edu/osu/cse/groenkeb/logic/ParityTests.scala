package edu.osu.cse.groenkeb.logic

import org.junit.Test
import org.junit.Assert

import dsl._

class ParityTests {
  @Test
  def testAtomicSentenceIsPositive {
    val a = Sentences.atom("a")
    Assert.assertEquals(Positive, a.parity(a))
  }
  
  @Test
  def testConjunctIsPositive {
    val a = Sentences.atom("a")
    val b = Sentences.atom("b")
    val ab = And(a,b)
    Assert.assertEquals(Positive, ab.parity(a))
    Assert.assertEquals(Positive, ab.parity(b))
  }
  
  @Test
  def testDisjunctIsPositive {
    val a = Sentences.atom("a")
    val b = Sentences.atom("b")
    val ab = Or(a,b)
    Assert.assertEquals(Positive, ab.parity(a))
    Assert.assertEquals(Positive, ab.parity(b))
  }
  
  @Test
  def testSimpleConditional {
    val a = Sentences.atom("a")
    val b = Sentences.atom("b")
    val ab = If(a,b)
    Assert.assertEquals(Negative, ab.parity(a))
    Assert.assertEquals(Positive, ab.parity(b))
  }
  
  @Test
  def testSimpleNegation {
    val a = Sentences.atom("a")
    val notA = Not(a)
    Assert.assertEquals(Negative, notA.parity(a))
  }
  
  @Test
  def testNestedConditional {
    val a = Sentences.atom("a")
    val b = Sentences.atom("b")
    val c = Sentences.atom("c")
    val d = Sentences.atom("d")
    val s = If(If(a,b),If(c,d))
    Assert.assertEquals(Positive, s.parity(a))
    Assert.assertEquals(Negative, s.parity(b))
    Assert.assertEquals(Negative, s.parity(c))
    Assert.assertEquals(Positive, s.parity(d))
  }
  
  @Test
  def testNestedNegation {
    val a = Sentences.atom("a")
    val b = Sentences.atom("b")
    val ab = If(a,b)
    val notAB = Not(ab)
    val notNotAB = Not(notAB)
    Assert.assertEquals(Negative, notNotAB.parity(a))
    Assert.assertEquals(Positive, notNotAB.parity(b))
    Assert.assertEquals(Positive, notNotAB.parity(ab))
    Assert.assertEquals(Negative, notNotAB.parity(notAB))
    Assert.assertEquals(Positive, notNotAB.parity(notNotAB))
  }
}