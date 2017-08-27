package edu.osu.cse.groenkeb.logic.proof

import org.junit.Assert
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.NamedPredicate
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.Term
import edu.osu.cse.groenkeb.logic.model.AtomicDiagram
import edu.osu.cse.groenkeb.logic.model.Domain
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.model.rules.AndFalsification
import edu.osu.cse.groenkeb.logic.model.rules.AndVerification
import edu.osu.cse.groenkeb.logic.model.rules.ModelRule
import edu.osu.cse.groenkeb.logic.model.rules.NegationVerification
import edu.osu.cse.groenkeb.logic.model.rules.OrFalsification
import edu.osu.cse.groenkeb.logic.model.rules.OrVerification
import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise
import edu.osu.cse.groenkeb.logic.model.rules.NegationFalsification


class ModelVerificationTests {
  val _name = new TestName()
  
  @Rule
  def name = _name
  
  @Before
  def setup {
    println("---- " + name.getMethodName() + " ----")
  }
  
  @Test
  def testVerifyTrivial() {
    val termA = Term("a")
    val domain = Domain(termA)
    val diagram = AtomicDiagram(domain, ObjectRelation(NamedPredicate("R"), termA))
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model)))
    val context = ProofContext(Sentences.atom("R[a]"), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyTrivial() {
    val termA = Term("a")
    val domain = Domain()
    val diagram = AtomicDiagram(domain)
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model)))
    val context = ProofContext(Absurdity(), rules, Seq(ProudPremise(Sentences.atom("F[a]"))))
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyAnd() {
    val termA = Term("a")
    val predR = NamedPredicate("R")
    val predQ = NamedPredicate("Q")
    val domain = Domain(termA)
    val diagram = AtomicDiagram(domain, ObjectRelation(predR, termA), ObjectRelation(predQ, termA))
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model), AndVerification()))
    val context = ProofContext(Sentences.and(Sentences.atom("R[a]"), Sentences.atom("Q[a]")), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyAnd() {
    val termA = Term("a")
    val predR = NamedPredicate("R")
    val predQ = NamedPredicate("Q")
    val domain = Domain()
    val diagram = AtomicDiagram(domain)
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model), AndFalsification()))
    val context = ProofContext(Absurdity(), rules, Seq(ProudPremise(Sentences.and(Sentences.atom("R[a]"), Sentences.atom("Q[a]")))))
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyAndDeep() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(and R[a] (and R[b] (and R[c] R[d])))"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel.from(parser.parse("R[a]"), parser.parse("R[b]"), parser.parse("R[c]"), parser.parse("R[d]"))
    val rules = RuleSet(Seq(ModelRule(model), AndVerification()))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyOr() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(or R[a] R[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel.from(parser.parse("R[a]"))
    val rules = RuleSet(Seq(ModelRule(model), OrVerification()))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyOr() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(or R[a] R[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel()
    val rules = RuleSet(Seq(ModelRule(model), OrFalsification()))
    val context = ProofContext(Absurdity(), rules, Seq(ProudPremise(parser.parse(sentence))))
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyNegation() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(not R[a])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel()
    val rules = RuleSet(Seq(ModelRule(model), NegationVerification()))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyNegation() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(not R[a])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel.from(parser.parse("R[a]"))
    val rules = RuleSet(Seq(ModelRule(model), NegationFalsification()))
    val context = ProofContext(Absurdity(), rules, Seq(ProudPremise(parser.parse(sentence))))
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
}