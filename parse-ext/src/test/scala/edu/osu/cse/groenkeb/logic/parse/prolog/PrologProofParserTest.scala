package edu.osu.cse.groenkeb.logic.parse.prolog

import org.junit.Test

@Test
final class PrologProofParserTest {
  
  @Test
  def testTrivialProof_1() {
    val parser = PrologProofParser
    val proofTerm = "d([a],a,a)"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_2() {
    val parser = PrologProofParser
    val proofTerm = "d([not(a)],not(a),not(a))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_3() {
    val parser = PrologProofParser
    val proofTerm = "d([and(a,b)],and(a,b),and(a,b))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_4() {
    val parser = PrologProofParser
    val proofTerm = "d([if(a,b)],if(a,b),if(a,b))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_5() {
    val parser = PrologProofParser
    val proofTerm = "d([or(a,b)],or(a,b),or(a,b))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testDoubleNegationProof() {
    val parser = PrologProofParser
    val proofTerm = "d([a],not(not(a)),not_i(d([a,not(a)],#,not_e(not(a),d([a],a,a)))))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testAndIntroProof() {
    val parser = PrologProofParser
    val proofTerm = "d([a,b],and(a,b),and_i(d([a],a,a),d([b],b,b)))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testArrowElimProof() {
    val parser = PrologProofParser
    val proofTerm = "d([a,if(a,b)],b,if_e(if(a,b),d([a],a,a),d([b],b,b)))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testArrowIntroProof_2() {
    val parser = PrologProofParser
    val proofTerm = "d([],if(a,if(a,a)),if_i(d([],if(a,a),if_i(d([a],a,a)))))"
    println(parser.parse(proofTerm, Nil))
  }
}