package edu.osu.cse.groenkeb.logic.parse.prolog

import org.junit.Test

import edu.osu.cse.groenkeb.logic.parse.corepl.CorePLProofParser;

@Test
final class CorePLProofParserTest {
  
  @Test
  def testTrivialProof_1() {
    val parser = CorePLProofParser
    val proofTerm = "d([a],a,a)"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_2() {
    val parser = CorePLProofParser
    val proofTerm = "d([not(a)],not(a),not(a))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_3() {
    val parser = CorePLProofParser
    val proofTerm = "d([and(a,b)],and(a,b),and(a,b))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_4() {
    val parser = CorePLProofParser
    val proofTerm = "d([if(a,b)],if(a,b),if(a,b))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testTrivialProof_5() {
    val parser = CorePLProofParser
    val proofTerm = "d([or(a,b)],or(a,b),or(a,b))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testDoubleNegationProof() {
    val parser = CorePLProofParser
    val proofTerm = "d([a],not(not(a)),not_i(d([a,not(a)],#,not_e(not(a),d([a],a,a)))))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testAndIntroProof() {
    val parser = CorePLProofParser
    val proofTerm = "d([a,b],and(a,b),and_i(d([a],a,a),d([b],b,b)))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testArrowElimProof() {
    val parser = CorePLProofParser
    val proofTerm = "d([a,if(a,b)],b,if_e(if(a,b),d([a],a,a),d([b],b,b)))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testArrowIntroProof_2() {
    val parser = CorePLProofParser
    val proofTerm = "d([],if(a,if(a,a)),if_i(d([],if(a,a),if_i(d([a],a,a)))))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testProblem_89() {
    val parser = CorePLProofParser
    val proofTerm = "d([or(a,b),not(a)],not(not(b)),or_e(or(a,b),d([a,not(a)],#,not_e(not(a),d([a],a,a))),d([b],not(not(b)),not_i(d([b,not(b)],#,not_e(not(b),d([b],b,b)))))))"
    println(parser.parse(proofTerm, Nil))
  }
  
  @Test
  def testProblem_140() {
    val parser = CorePLProofParser
    val proofTerm = "d([],if(and(and(or(p,q),or(not(p),q)),or(p,not(q))),not(or(not(p),not(q)))),if_i(d([and(and(or(p,q),or(not(p),q)),or(p,not(q)))],not(or(not(p),not(q))),and_e(and(and(or(p,q),or(not(p),q)),or(p,not(q))),d([and(or(p,q),or(not(p),q)),or(p,not(q))],not(or(not(p),not(q))),and_e(and(or(p,q),or(not(p),q)),d([or(not(p),q),or(p,q),or(p,not(q))],not(or(not(p),not(q))),or_e(or(not(p),q),d([or(p,q),not(p),or(p,not(q))],not(or(not(p),not(q))),or_e(or(p,q),d([p,not(p)],#,not_e(not(p),d([p],p,p))),d([or(p,not(q)),q],not(or(not(p),not(q))),or_e(or(p,not(q)),d([p,q],not(or(not(p),not(q))),not_i(d([or(not(p),not(q)),p,q],#,or_e(or(not(p),not(q)),d([p,not(p)],#,not_e(not(p),d([p],p,p))),d([q,not(q)],#,not_e(not(q),d([q],q,q))))))),d([q,not(q)],#,not_e(not(q),d([q],q,q))))))),d([or(p,q),q,or(p,not(q))],not(or(not(p),not(q))),or_e(or(p,q),d([p,q],not(or(not(p),not(q))),not_i(d([or(not(p),not(q)),p,q],#,or_e(or(not(p),not(q)),d([p,not(p)],#,not_e(not(p),d([p],p,p))),d([q,not(q)],#,not_e(not(q),d([q],q,q))))))),d([or(p,not(q)),q],not(or(not(p),not(q))),or_e(or(p,not(q)),d([p,q],not(or(not(p),not(q))),not_i(d([or(not(p),not(q)),p,q],#,or_e(or(not(p),not(q)),d([p,not(p)],#,not_e(not(p),d([p],p,p))),d([q,not(q)],#,not_e(not(q),d([q],q,q))))))),d([q,not(q)],#,not_e(not(q),d([q],q,q)))))))))))))))"
    println(parser.parse(proofTerm, Nil))
  }
}