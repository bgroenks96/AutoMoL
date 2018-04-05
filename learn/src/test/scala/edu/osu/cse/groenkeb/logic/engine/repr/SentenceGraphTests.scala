package edu.osu.cse.groenkeb.logic.engine.repr

import org.junit.Test
import org.junit.Assert
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.engine.repr._

final class SentenceGraphTests {
  @Test
  def testAtomicSentenceToGraph() {
    // arrange
    val term = Term("a")
    val atom = Atom(NamedPredicate("R"), term)
    val s = AtomicSentence(atom)
    
    // act
    val graph = SentenceGraph(s)
    
    // assert
    Assert.assertEquals(2, graph.size)
    Assert.assertTrue(graph.adj.contains(AtomicNode(s)))
    Assert.assertTrue(graph.adj.contains(VarNode(term)))
    Assert.assertTrue(graph.adj(AtomicNode(s)).equals(Set(VarNode(term))))
    Assert.assertTrue(graph.adj(VarNode(term)).isEmpty)
    println(graph)
  }
  
  @Test
  def testUnarySentenceToGraph_Not() {
    // arrange
    val termA = Term("a")
    val atomR = AtomicSentence(Atom(NamedPredicate("R"), termA))
    val s = Not(atomR)
    
    // act
    val graph = SentenceGraph(s)
    
    // assert
    Assert.assertEquals(3, graph.size)
    Assert.assertTrue(graph.adj.contains(UnaryNode(s)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomR)))
    Assert.assertTrue(graph.adj.contains(VarNode(termA)))
    Assert.assertTrue(graph.adj(UnaryNode(s)).equals(Set(AtomicNode(atomR))))
    Assert.assertTrue(graph.adj(AtomicNode(atomR)).equals(Set(VarNode(termA))))
    Assert.assertTrue(graph.adj(VarNode(termA)).isEmpty)
    println(graph)
  }
  
  @Test
  def testBinarySentenceToGraph_And() {
    // arrange
    val termA = Term("a")
    val termB = Term("b")
    val atomR = AtomicSentence(Atom(NamedPredicate("R"), termA))
    val atomQ = AtomicSentence(Atom(NamedPredicate("Q"), termB))
    val s = And(atomR, atomQ)
    
    // act
    val graph = SentenceGraph(s)
    
    // assert
    Assert.assertEquals(5, graph.size)
    Assert.assertTrue(graph.adj.contains(BinaryNode(s)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomR)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomQ)))
    Assert.assertTrue(graph.adj.contains(VarNode(termA)))
    Assert.assertTrue(graph.adj.contains(VarNode(termB)))
    Assert.assertTrue(graph.adj(BinaryNode(s)).equals(Set(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adj(AtomicNode(atomR)).equals(Set(VarNode(termA))))
    Assert.assertTrue(graph.adj(AtomicNode(atomQ)).equals(Set(VarNode(termB))))
    Assert.assertTrue(graph.adj(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adj(VarNode(termB)).isEmpty)
    println(graph)
  }
  
  @Test
  def testBinarySentenceToGraph_Or() {
    // arrange
    val termA = Term("a")
    val termB = Term("b")
    val atomR = AtomicSentence(Atom(NamedPredicate("R"), termA))
    val atomQ = AtomicSentence(Atom(NamedPredicate("Q"), termB))
    val s = Or(atomR, atomQ)
    
    // act
    val graph = SentenceGraph(s)
    
    // assert
    Assert.assertEquals(5, graph.size)
    Assert.assertTrue(graph.adj.contains(BinaryNode(s)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomR)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomQ)))
    Assert.assertTrue(graph.adj.contains(VarNode(termA)))
    Assert.assertTrue(graph.adj.contains(VarNode(termB)))
    Assert.assertTrue(graph.adj(BinaryNode(s)).equals(Set(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adj(AtomicNode(atomR)).equals(Set(VarNode(termA))))
    Assert.assertTrue(graph.adj(AtomicNode(atomQ)).equals(Set(VarNode(termB))))
    Assert.assertTrue(graph.adj(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adj(VarNode(termB)).isEmpty)
    println(graph)
  }
  
  @Test
  def testBinarySentenceToGraph_And_DepthOf2() {
    // arrange
    val termA = Term("a")
    val termB = Term("b")
    val atomR = AtomicSentence(Atom(NamedPredicate("R"), termA))
    val atomQ = AtomicSentence(Atom(NamedPredicate("Q"), termB))
    var andRQ = And(atomR, atomQ)
    val s = And(andRQ, andRQ)
    
    // act
    val graph = SentenceGraph(s)
    
    // assert
    Assert.assertEquals(6, graph.size)
    Assert.assertTrue(graph.adj.contains(BinaryNode(s)))
    Assert.assertTrue(graph.adj.contains(BinaryNode(andRQ)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomR)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomQ)))
    Assert.assertTrue(graph.adj.contains(VarNode(termA)))
    Assert.assertTrue(graph.adj.contains(VarNode(termB)))
    Assert.assertTrue(graph.adj(BinaryNode(s)).equals(Set(BinaryNode(andRQ))))
    Assert.assertTrue(graph.adj(BinaryNode(andRQ)).equals(Set(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adj(AtomicNode(atomR)).equals(Set(VarNode(termA))))
    Assert.assertTrue(graph.adj(AtomicNode(atomQ)).equals(Set(VarNode(termB))))
    Assert.assertTrue(graph.adj(VarNode(termA)).isEmpty)
    Assert.assertTrue(graph.adj(VarNode(termB)).isEmpty)
    println(graph)
  }
  
  @Test
  def testQuantifiedSentenceToGraph() {
    // arrange
    val termX = Term("x")
    val termA = Term("a")
    val atomR = AtomicSentence(Atom(NamedPredicate("R"), termX))
    val atomQ = AtomicSentence(Atom(NamedPredicate("Q"), termA))
    val orRQ = Or(atomR, atomQ)
    val s = QuantifiedSentence(orRQ, ExistentialQuantifier(termX))
    
    // act
    val graph = SentenceGraph(s)
    
    // assert
    Assert.assertEquals(6, graph.size)
    Assert.assertTrue(graph.adj.contains(QuantifierNode(s)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomR)))
    Assert.assertTrue(graph.adj.contains(AtomicNode(atomQ)))
    Assert.assertTrue(graph.adj.contains(VarNode(termX)))
    Assert.assertTrue(graph.adj(QuantifierNode(s)).equals(Set(BinaryNode(orRQ), VarNode(termX))))
    Assert.assertTrue(graph.adj(BinaryNode(orRQ)).equals(Set(AtomicNode(atomR), AtomicNode(atomQ))))
    Assert.assertTrue(graph.adj(AtomicNode(atomR)).equals(Set(VarNode(termX))))
    Assert.assertTrue(graph.adj(AtomicNode(atomQ)).equals(Set(VarNode(termA))))
    Assert.assertTrue(graph.adj(VarNode(termX)).isEmpty)
    Assert.assertTrue(graph.adj(VarNode(termA)).isEmpty)
    println(graph)
  }
}