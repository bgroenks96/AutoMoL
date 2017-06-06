package edu.osu.cse.groenkeb.logic.proof

import org.junit.Assert
import org.junit.Test

import edu.osu.cse.groenkeb.logic.NamedPredicate
import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.Term
import edu.osu.cse.groenkeb.logic.model.AtomicDiagram
import edu.osu.cse.groenkeb.logic.model.Domain
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel
import edu.osu.cse.groenkeb.logic.model.rules.ModelRule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet
import edu.osu.cse.groenkeb.logic.model.rules.AndVerification
import org.junit.Rule
import org.junit.rules.TestName
import org.junit.Before


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
    implicit val context = ProofContext(Sentences.atom("R[a]"), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.proofs.collect { case r:Success => r.asInstanceOf[Success] }
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
    implicit val context = ProofContext(Sentences.and(Sentences.atom("R[a]"), Sentences.atom("Q[a]")), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.proofs.collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
  
  @Test
  def testFalsifyAnd() {
    val termA = Term("a")
    val predR = NamedPredicate("R")
    val predQ = NamedPredicate("Q")
    val domain = Domain(termA)
    val diagram = AtomicDiagram(domain, ObjectRelation(predR, termA), ObjectRelation(predQ, termA))
    val model = FirstOrderModel(diagram)
    val rules = RuleSet(Seq(ModelRule(model), AndVerification()))
    implicit val context = ProofContext(Sentences.and(Sentences.atom("R[a]"), Sentences.atom("Q[a]")), rules, Nil)
    val solver = new ProofSolver(new NaiveProofStrategy())
    val results = solver.proofs.collect { case r:Success => r.asInstanceOf[Success] }
    Assert.assertFalse(results.isEmpty)
    ProofUtils.prettyPrint(results.head.proof)
  }
}