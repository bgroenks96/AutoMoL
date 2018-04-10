package edu.osu.cse.groenkeb.logic.engine.learn

import org.junit.Test
import org.junit.Assert
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.engine.learn._

final class SentenceGraphTests {
  @Test
  def testAtomicSentenceToGraph() {
    // arrange
    val term = Term("a")
    val atom = Atom(NamedPredicate("R"), term)
    
    // act
    val (graph, _) = SentenceGraph(AtomicSentence(atom))
    
    // assert
    println(graph)
    Assert.assertTrue(graph.has(AtomicNode(atom)))
    Assert.assertTrue(graph.has(PredicateNode(atom.predicate)))
    Assert.assertTrue(graph.has(VarNode(term)))
    Assert.assertTrue(graph.adjOut(AtomicNode(atom)).equals(Seq(PredicateNode(atom.predicate), VarNode(term))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atom)).isEmpty)
    Assert.assertTrue(graph.adjOut(PredicateNode(atom.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atom.predicate)).equals(Seq(AtomicNode(atom))))
    Assert.assertTrue(graph.adjOut(VarNode(term)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(term)).equals(Seq(AtomicNode(atom))))
  }
  
  @Test
  def testUnarySentenceToGraph_Not() {
    // arrange
    val termA = Term("a")
    val atomR = Atom(NamedPredicate("R"), termA)
    val s = Not(AtomicSentence(atomR))
    
    // act
    val (graph, _) = SentenceGraph(s)
    
    // assert
    println(graph)
    Assert.assertTrue(graph.has(UnaryNode(s)))
    Assert.assertTrue(graph.has(AtomicNode(atomR)))
    Assert.assertTrue(graph.has(PredicateNode(atomR.predicate)))
    Assert.assertTrue(graph.has(VarNode(termA)))
    Assert.assertTrue(graph.adjOut(UnaryNode(s)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjIn(UnaryNode(s)).isEmpty)
    Assert.assertTrue(graph.adjOut(AtomicNode(atomR)).equals(Seq(PredicateNode(atomR.predicate), VarNode(termA))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomR)).equals(Seq(UnaryNode(s))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomR.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomR.predicate)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termA)).equals(Seq(AtomicNode(atomR))))
  }
  
  @Test
  def testBinarySentenceToGraph_And() {
    // arrange
    val termA = Term("a")
    val termB = Term("b")
    val atomR = Atom(NamedPredicate("R"), termA)
    val atomQ = Atom(NamedPredicate("Q"), termB)
    val s = And(AtomicSentence(atomR), AtomicSentence(atomQ))
    
    // act
    val (graph, _) = SentenceGraph(s)
    
    // assert
    println(graph)
    Assert.assertTrue(graph.has(BinaryNode(s)))
    Assert.assertTrue(graph.has(AtomicNode(atomR)))
    Assert.assertTrue(graph.has(AtomicNode(atomQ)))
    Assert.assertTrue(graph.has(VarNode(termA)))
    Assert.assertTrue(graph.has(VarNode(termB)))
    Assert.assertTrue(graph.adjOut(BinaryNode(s)).equals(Seq(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjIn(BinaryNode(s)).isEmpty)
    Assert.assertTrue(graph.adjOut(AtomicNode(atomR)).equals(Seq(PredicateNode(atomR.predicate), VarNode(termA))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomR)).equals(Seq(BinaryNode(s))))
    Assert.assertTrue(graph.adjOut(AtomicNode(atomQ)).equals(Seq(PredicateNode(atomQ.predicate), VarNode(termB))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomQ)).equals(Seq(BinaryNode(s))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomR.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomR.predicate)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomQ.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomQ.predicate)).equals(Seq(AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjOut(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termA)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(VarNode(termB)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termB)).equals(Seq(AtomicNode(atomQ))))
  }
  
  @Test
  def testBinarySentenceToGraph_Or() {
    // arrange
    val termA = Term("a")
    val termB = Term("b")
    val atomR = Atom(NamedPredicate("R"), termA)
    val atomQ = Atom(NamedPredicate("Q"), termB)
    val s = Or(AtomicSentence(atomR), AtomicSentence(atomQ))
    
    // act
    val (graph, _) = SentenceGraph(s)
    
    // assert
    Assert.assertTrue(graph.has(BinaryNode(s)))
    Assert.assertTrue(graph.has(AtomicNode(atomR)))
    Assert.assertTrue(graph.has(AtomicNode(atomQ)))
    Assert.assertTrue(graph.has(VarNode(termA)))
    Assert.assertTrue(graph.has(VarNode(termB)))
    Assert.assertTrue(graph.adjOut(BinaryNode(s)).equals(Seq(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjIn(BinaryNode(s)).isEmpty)
    Assert.assertTrue(graph.adjOut(AtomicNode(atomR)).equals(Seq(PredicateNode(atomR.predicate), VarNode(termA))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomR)).equals(Seq(BinaryNode(s))))
    Assert.assertTrue(graph.adjOut(AtomicNode(atomQ)).equals(Seq(PredicateNode(atomQ.predicate), VarNode(termB))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomQ)).equals(Seq(BinaryNode(s))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomR.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomR.predicate)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomQ.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomQ.predicate)).equals(Seq(AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjOut(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termA)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(VarNode(termB)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termB)).equals(Seq(AtomicNode(atomQ))))
    println(graph)
  }
  
  @Test
  def testBinarySentenceToGraph_And_DepthOf2() {
    // arrange
    val termA = Term("a")
    val termB = Term("b")
    val atomR = Atom(NamedPredicate("R"), termA)
    val atomQ = Atom(NamedPredicate("Q"), termB)
    var andRQ = And(AtomicSentence(atomR), AtomicSentence(atomQ))
    val s = And(andRQ, andRQ)
    
    // act
    val (graph, _) = SentenceGraph(s)
    
    // assert
    println(graph)
    Assert.assertTrue(graph.has(BinaryNode(s)))
    Assert.assertTrue(graph.has(BinaryNode(andRQ)))
    Assert.assertTrue(graph.has(AtomicNode(atomR)))
    Assert.assertTrue(graph.has(AtomicNode(atomQ)))
    Assert.assertTrue(graph.has(VarNode(termA)))
    Assert.assertTrue(graph.has(VarNode(termB)))
    Assert.assertTrue(graph.adjOut(BinaryNode(s)).equals(Seq(BinaryNode(andRQ), BinaryNode(andRQ))))
    Assert.assertTrue(graph.adjIn(BinaryNode(s)).isEmpty)
    Assert.assertTrue(graph.adjOut(BinaryNode(andRQ)).equals(Seq(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjIn(BinaryNode(andRQ)).equals(Seq(BinaryNode(s))))
    Assert.assertTrue(graph.adjOut(AtomicNode(atomR)).equals(Seq(PredicateNode(atomR.predicate), VarNode(termA))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomR)).equals(Seq(BinaryNode(andRQ))))
    Assert.assertTrue(graph.adjOut(AtomicNode(atomQ)).equals(Seq(PredicateNode(atomQ.predicate), VarNode(termB))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomQ)).equals(Seq(BinaryNode(andRQ))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomR.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomR.predicate)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomQ.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomQ.predicate)).equals(Seq(AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjOut(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termA)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(VarNode(termB)).isEmpty)
    Assert.assertTrue(graph.adjIn(VarNode(termB)).equals(Seq(AtomicNode(atomQ))))
  }
  
  @Test
  def testQuantifiedSentenceToGraph() {
    // arrange
    val termX = Term("x")
    val termA = Term("a")
    val atomR = Atom(NamedPredicate("R"), termX)
    val atomQ = Atom(NamedPredicate("Q"), termA)
    val orRQ = Or(AtomicSentence(atomR), AtomicSentence(atomQ))
    val s = QuantifiedSentence(orRQ, ExistentialQuantifier(termX))
    
    // act
    val (graph, _) = SentenceGraph(s)
    
    // assert
    println(graph)
    Assert.assertTrue(graph.has(QuantifierNode(s)))
    Assert.assertTrue(graph.has(AtomicNode(atomR)))
    Assert.assertTrue(graph.has(AtomicNode(atomQ)))
    Assert.assertTrue(graph.has(VarNode(termX)))
    Assert.assertTrue(graph.adjOut(QuantifierNode(s)).equals(Seq(VarNode(termX), BinaryNode(orRQ))))
    Assert.assertTrue(graph.adjIn(QuantifierNode(s)).isEmpty)
    Assert.assertTrue(graph.adjOut(BinaryNode(orRQ)).equals(Seq(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjIn(BinaryNode(orRQ)).equals(Seq(QuantifierNode(s))))
    Assert.assertTrue(graph.adjOut(AtomicNode(atomR)).equals(Seq(PredicateNode(atomR.predicate), VarNode(termX))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomR)).equals(Seq(BinaryNode(orRQ))))
    Assert.assertTrue(graph.adjOut(AtomicNode(atomQ)).equals(Seq(PredicateNode(atomQ.predicate), VarNode(termA))))
    Assert.assertTrue(graph.adjIn(AtomicNode(atomQ)).equals(Seq(BinaryNode(orRQ))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomR.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomR.predicate)).equals(Seq(AtomicNode(atomR))))
    Assert.assertTrue(graph.adjOut(PredicateNode(atomQ.predicate)).isEmpty)
    Assert.assertTrue(graph.adjIn(PredicateNode(atomQ.predicate)).equals(Seq(AtomicNode(atomQ))))
    Assert.assertTrue(graph.adjOut(VarNode(termX)).isEmpty)
    Assert.assertTrue(graph.adjOut(VarNode(termA)).isEmpty)
  }
}