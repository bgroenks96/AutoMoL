package edu.osu.cse.groenkeb.logic.engine.learn.q

import scala.collection.immutable.Seq

import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.rules.TestName

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.And
import edu.osu.cse.groenkeb.logic.If
import edu.osu.cse.groenkeb.logic.Not
import edu.osu.cse.groenkeb.logic.Or
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.ProofUtils
import edu.osu.cse.groenkeb.logic.proof.engine.ProofSolver
import edu.osu.cse.groenkeb.logic.proof.engine.Success
import edu.osu.cse.groenkeb.logic.proof.engine.Trace
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.rules.core.AndElimination
import edu.osu.cse.groenkeb.logic.proof.rules.core.AndIntroduction
import edu.osu.cse.groenkeb.logic.proof.rules.core.IfElimination
import edu.osu.cse.groenkeb.logic.proof.rules.core.IfIntroduction
import edu.osu.cse.groenkeb.logic.proof.rules.core.NegationElimination
import edu.osu.cse.groenkeb.logic.proof.rules.core.NegationIntroduction
import edu.osu.cse.groenkeb.logic.proof.rules.core.OrElimination
import edu.osu.cse.groenkeb.logic.proof.rules.core.OrIntroduction
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.QLearningStrategy
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.Features
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.LinearQModel
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.EpsilonGreedy

class QBasicCoreProofTests {
  val _name = new TestName()
  
  val gamma = 0.9
  val policy = new EpsilonGreedy(0.5, decay=0.01f)
  val features = Features.concMatching +: Features.ruleMatching(standardRules)
  val model = new LinearQModel(features, gamma)
  implicit val strategy = new QLearningStrategy(model, policy)
  implicit val trace = Trace()
  implicit val options = Seq(trace)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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
    println(trace.stepCount)
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