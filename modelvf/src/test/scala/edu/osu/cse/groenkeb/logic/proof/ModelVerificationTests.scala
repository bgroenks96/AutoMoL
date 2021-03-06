package edu.osu.cse.groenkeb.logic.proof

import org.junit.Assert
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TestName

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.model._
import edu.osu.cse.groenkeb.logic.model.rules._
import edu.osu.cse.groenkeb.logic.parse._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.rules._

import scala.collection.immutable.Seq

class ModelVerificationTests {
  val _name = new TestName()
  
  implicit val strategy = new EvaluationProofStrategy()
  implicit val options = Seq(Trace)
  
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
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyTrivial() {
    val domain = Domain(Term("a"))
    val diagram = AtomicDiagram(domain, emptyMonadicRelation("F"))
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model)))
    val context = ProofContext(Absurdity, rules, Seq(Assumption(Sentences.atom("F[a]"))))
    val solver = new ProofSolver
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
    val rules = RuleSet(Seq(ModelRule(model), AndVerification))
    val context = ProofContext(Sentences.and(Sentences.atom("R[a]"), Sentences.atom("Q[a]")), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
    println(results.head.proof)
  }
  
  @Test
  def testFalsifyAnd() {
    val domain = Domain(Term("a"))
    val diagram = AtomicDiagram(domain, emptyMonadicRelation("R"), emptyMonadicRelation("Q"))
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model), AndFalsification))
    val context = ProofContext(Absurdity, rules, Seq(Assumption(Sentences.and(Sentences.atom("R[a]"), Sentences.atom("Q[a]")))))
    val solver = new ProofSolver
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
    val rules = RuleSet(Seq(ModelRule(model), AndVerification))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyAndDeep() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(and (and R[a] R[b]) (and R[c] R[d]))"
    val parser = SentenceParser(NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a"), Term("b"), Term("c"), Term("d")), emptyMonadicRelation("R")))
    val rules = RuleSet(Seq(ModelRule(model), AndFalsification))
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
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
    val rules = RuleSet(Seq(ModelRule(model), OrVerification))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyOr() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(or R[a] R[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a"), Term("b")), emptyMonadicRelation("R")))
    val rules = RuleSet(Seq(ModelRule(model), OrFalsification))
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyNegation() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(not R[a])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a"), Term("b")), emptyMonadicRelation("R")))
    val rules = RuleSet(Seq(ModelRule(model), NegationVerification))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
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
    val rules = RuleSet(Seq(ModelRule(model), NegationFalsification))
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyConditionalMethod1() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(if R[a] R[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel.from(parser.parse("R[a]"), parser.parse("R[b]"))
    val rules = RuleSet(Seq(ModelRule(model), ConditionalVerification))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyConditionalMethod2() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(if R[a] R[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a"), Term("b")), emptyMonadicRelation("R")));
    val rules = RuleSet(Seq(ModelRule(model), ConditionalVerification))
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyConditional() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(if R[a] R[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("Q[b]"));
    val rules = RuleSet(Seq(ModelRule(model), ConditionalFalsification))
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testSimpleVerificationProof1() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(and R[b] (if R[a] Q[a]))"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("R[b]"), parser.parse("Q[a]"));
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testSimpleVerificationProof2() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(or (and R[a] Q[b]) (if R[a] Q[a]))"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("Q[a]"));
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testSimpleVerificationProof3_LEM() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(or F[a] (not F[a]))"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a")), emptyMonadicRelation("F")))
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Nil)
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testSimpleFalsificationProof1() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(if (and R[a] R[b]) Q[b])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("R[b]"), parser.parse("Q[a]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testSimpleFalsificationProof2() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "or Q[b] (if Q[a] R[c])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("R[b]"), parser.parse("Q[a]"), parser.parse("Q[c]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyUniversal() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ux (or R[x] Q[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("R[b]"), parser.parse("Q[a]"));
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Seq())
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyUniversal() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ux R[x]"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("Q[b]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyExistential() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ex (or R[x] Q[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"));
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Seq())
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyExistentialWithFreeVariable() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ex (or Q[d] R[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("R[a]"), parser.parse("Q[b]"), parser.parse("Q[c]"));
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Seq())
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyExistential() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ex R[x]"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a"), Term("b")), emptyMonadicRelation("R")));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyExistentialBoundVariableNotUsed() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ex R[a]"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a")), emptyMonadicRelation("R")));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyExistentialWithNestedFalsification_1() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ex (and Q[x] R[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("Q[a]"), parser.parse("Q[b]"), parser.parse("R[c]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyExistentialWithNestedFalsification_2() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "Ex (and (not Q[x]) Q[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("Q[a]"), parser.parse("Q[b]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyNestedExistential() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "not (Ex Q[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("Q[a]"), parser.parse("Q[b]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyNestedExistentialWithNestedFalsification() {
    implicit val opMatcher = new DefaultFirstOrderOpMatcher()
    val sentence = "and (Ex (and Q[x] R[x])) (Ex Q[x])"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(parser.parse("Q[a]"), parser.parse("Q[b]"), parser.parse("R[c]"));
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testVerifyNegationNestedWithAllRules() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(not (not (not R[a])))"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a")), emptyMonadicRelation("R")))
    val rules = standardRules(model)
    val context = ProofContext(parser.parse(sentence), rules, Seq())
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyNegationNestedWithAllRules() {
    implicit val opMatcher = new DefaultPropOpMatcher()
    val sentence = "(not (not (not (not R[a]))))"
    val parser = new SentenceParser(new NodeRecursiveTokenizer())
    val model = FirstOrderModel(AtomicDiagram(Domain(Term("a")), emptyMonadicRelation("R")))
    val rules = standardRules(model)
    val context = ProofContext(Absurdity, rules, Seq(Assumption(parser.parse(sentence))))
    val solver = new ProofSolver
    val results = solver.prove(context).collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  private def emptyMonadicRelation(name: String) = EmptyRelation(NamedPredicate(name), 1)
  
  private def emptyDiadicRelation(name: String) = EmptyRelation(NamedPredicate(name), 2)
  
  private def standardRules(model: FirstOrderModel) =
    RuleSet(Seq(ModelRule(model),
    NegationVerification, NegationFalsification,
    AndVerification, AndFalsification,
    OrVerification, OrFalsification,
    ConditionalVerification, ConditionalFalsification,
    UniversalVerification(model.domain), UniversalFalsification(model.domain),
    ExistentialVerification(model.domain), ExistentialFalsification(model.domain)))
}
