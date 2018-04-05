package edu.osu.cse.groenkeb.logic.proof.rules.core

import java.io.BufferedReader
import java.io.InputStreamReader

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.immutable.Seq

import org.junit.Test
import org.junit.Assert

import atto._
import atto.Atto._
import atto.ParseResult._
import cats.implicits._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.parse.ParserException

class PosQuestionsTests {
  
  @Test
  def testPosquestionsNaiveStrategy = {
    val solver = new ProofSolver()
    val rules = standardRules
    val questions = loadPosquestions.toList
    val proofs = for {
      (assumptions, goal) <- questions
    } yield {
      solver.prove(ProofContext(goal, rules, assume(assumptions:_*))).collect({ case r:Success => r })
    }
    
    proofs.zipWithIndex.foreach {
      case (results, i) =>
        println("Problem " + (i + 1))
        Assert.assertFalse(s"Failed on problem $i; ${questions(i)}", results.isEmpty)
        ProofUtils.prettyPrint(results.head.proof);
        println("--------------")
    }
  }
  
  @Test
  def testPosquestionsWithStrategy = {
    implicit val strategy = new CoreProofStrategy()
    val solver = new ProofSolver()
    val rules = standardRules
    val questions = loadPosquestions.toList
    val proofs = for {
      (assumptions, goal) <- questions
    } yield {
      solver.prove(ProofContext(goal, rules, assume(assumptions:_*))).collect({ case r:Success => r })
    }
    
    proofs.zipWithIndex.foreach {
      case (results, i) =>
        println("Problem " + (i + 1))
        Assert.assertFalse(s"Failed on problem $i; ${questions(i)}", results.isEmpty)
        ProofUtils.prettyPrint(results.head.proof);
        println("--------------")
    }
  }
  
  private def upcast(sentence: Sentence) = sentence
  
  lazy val list: Parser[List[Sentence]] = {
    (char('[') ~> innerList <~ char(']')) |
    string("[]").map { _ => Nil }
  }
    
  lazy val innerList: Parser[List[Sentence]] = (
    for {
      head <- sentence
      _ <- char(',')
      tail <- innerList
    } yield List(head) ++ tail
  ) | sentence.map { s => List(s) }
    
  lazy val sentence: Parser[Sentence] = (
    (string("not(") ~> sentence <~ string(")")).map { s => Not(s) } |
    (string("and(") ~> sentence <~ char(','), sentence <~ char(')')).mapN { (left, right) => upcast(And(left, right)) } |
    (string("or(") ~> sentence <~ char(','), sentence <~ char(')')).mapN { (left, right) => upcast(Or(left, right)) } |
    (string("if(") ~> sentence <~ char(','), sentence <~ char(')')).mapN { (left, right) => upcast(If(left, right)) } |
    char('#').map { _ => upcast(Absurdity) } |
    stringOf(letter).map { name => AtomicSentence(Atom(NamedPredicate(name))) }
  )
  
  lazy val question: Parser[(List[Sentence], Sentence)] = (
    (list <~ string("?-"), sentence <~ char('.')).mapN { (premises, conc) => (premises, conc) }
  )
  
  private def loadPosquestions = {
    val res = ClassLoader.getSystemClassLoader.getResourceAsStream("posquestions")
    Assert.assertNotNull(res)
    val reader = new BufferedReader(new InputStreamReader(res))
    reader.lines().iterator().asScala
      .filterNot { s => s.trim().startsWith("%") }
      .filterNot { s => s.isEmpty() }
      .map { s => s.replaceAll("\\s+", "") }
      .map {
        s => question.parseOnly(s) match {
          case Done(_, res) => res
          case Fail(input, stack, msg) =>
            throw ParserException("error parsing input string: " + input + " | " + msg + "\n" + stack)
          case _ => ???
        }
      }
  }
  
  private def assume(sentences: Sentence*) = sentences.map { s => Assumption(s) }
    
  private def standardRules =
    RuleSet(Seq[Rule](
      NegationIntroduction, NegationElimination,
      AndIntroduction, AndElimination,
      OrIntroduction, OrElimination,
      IfIntroduction, IfElimination))
}
