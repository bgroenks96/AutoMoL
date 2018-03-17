package edu.osu.cse.groenkeb.logic.proof.rules.core

import scala.collection.immutable.Seq

import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.rules.TestName

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.parse._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._

class BasicCoreProofTests {
  val _name = new TestName()
  
  implicit val strategy = new NaiveProofStrategy()
  implicit val options = Seq(Trace)
  implicit val parser = new SentenceParser(new NodeRecursiveTokenizer())(new DefaultPropOpMatcher())
  
  @org.junit.Rule
  def name = _name
  
  @Before
  def setup {
    println("---- " + name.getMethodName() + " ----")
  }
  
  @Test
  def testTrivalProof() {
    val a = atom("a")
    val context = ProofContext(a, standardRules, assume(a))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testAndIntro() {
    val a = atom("a")
    val b = atom("b")
    val ab = And(a, b)
    val context = ProofContext(ab, standardRules, assume(a, b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testAndElim_1() {
    val a = atom("a")
    val b = atom("b")
    val ab = And(a, b)
    val context = ProofContext(a, standardRules, assume(ab))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testAndElim_2() {
    val a = atom("a")
    val b = atom("b")
    val ab = And(a, b)
    val context = ProofContext(b, standardRules, assume(ab))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testOrIntro() {
    val a = atom("a")
    val b = atom("b")
    val a_b = Or(a, b)
    val context = ProofContext(a_b, standardRules, assume(a))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testOrElim_1() {
    val a = atom("a")
    val b = atom("b")
    val not_b = Not(b)
    val a_b = Or(a, b)
    val context = ProofContext(a, standardRules, assume(a_b, not_b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testOrElim_2() {
    val a = atom("a")
    val b = atom("b")
    val not_a = Not(a)
    val not_b = Not(b)
    val a_b = Or(a, b)
    val context = ProofContext(Absurdity, standardRules, assume(a_b, not_a, not_b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testOrElim_3() {
    val a = atom("a")
    val b = atom("b")
    val not_b = Not(b)
    val a_b = Or(a, b)
    val context = ProofContext(a, standardRules, assume(a_b, not_b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testOrElim_4() {
    val a = atom("a")
    val b = atom("b")
    val not_a = Not(a)
    val not_b = Not(b)
    val a_b = Or(a, b)
    val context = ProofContext(b, standardRules, assume(a_b, not_a))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testNegIntro() {
    val a = atom("a")
    val b = atom("b")
    val not_a = Not(a)
    val not_b = Not(b)
    val ab = And(a, b)
    val if_na_nb = If(a, not_b)
    val context = ProofContext(not_a, standardRules, assume(if_na_nb, b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testNegElim() {
    val a = atom("a")
    val not_a = Not(a)
    val context = ProofContext(Absurdity, standardRules, assume(a, not_a))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testIfIntro_1() {
    val a = atom("a")
    val b = atom("b")
    val if_a_b = If(a, b)
    val context = ProofContext(if_a_b, standardRules, assume(b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testIfIntro_2() {
    val a = atom("a")
    val b = atom("b")
    val not_a = Not(a)
    val if_a_b = If(a, b)
    val context = ProofContext(if_a_b, standardRules, assume(not_a))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testIfElim() {
    val a = atom("a")
    val b = atom("b")
    val if_a_b = If(a, b)
    val context = ProofContext(b, standardRules, assume(a, if_a_b))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  private def assume(sentences: Sentence*) = sentences.map { s => Assumption(s) }
  
  private def atom(name: String) = Sentences.atom(name)
    
  private def standardRules =
    RuleSet(Seq[Rule](
      NegationIntroduction, NegationElimination,
      AndIntroduction, AndElimination,
      OrIntroduction, OrElimination,
      IfIntroduction, IfElimination))
}