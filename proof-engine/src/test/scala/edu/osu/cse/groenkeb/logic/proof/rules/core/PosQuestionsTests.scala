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
import edu.osu.cse.groenkeb.logic.parse.corepl.CorePLProofParser
import org.junit.Ignore

class PosQuestionsTests {
  
  @Ignore
  @Test
  def testPosquestionsNaiveStrategy = {
    implicit val trace = Trace()
    implicit val options = Seq(trace)
    implicit val rules = standardRules
    val solver = new ProofSolver()
    val questions = loadPosquestions.map { case (assumptions, goal) => ProofContext(goal, assumptions.map(s => Assumption(s))) }.toList
    var meanStepCount = 0.0
    questions.zipWithIndex.foreach {
      case (q, i) =>
        println("Problem " + (i + 1))
        val results = solver.prove(q).collect { case s:Success => s }
        Assert.assertFalse(s"Failed on problem $i; ${questions(i)}", results.isEmpty)
        val proof = results.head.proof
        ProofUtils.prettyPrint(proof);
        val proofSteps = ProofUtils.countSteps(proof)
        println(s"Found a proof with $proofSteps steps of inference after ${trace.stepCount} total attempted steps")
        println("--------------")
        meanStepCount += (trace.stepCount - meanStepCount) / (i+1)
    }
    
    println(s"Average step count: ${Math.round(meanStepCount)}")
  }
  
  @Test
  def testPosquestionsWithStrategy = {
    implicit val trace = Trace()
    implicit val options = Seq(trace)
    implicit val strategy = new CoreProofStrategy()
    implicit val rules = standardRules
    val solver = new ProofSolver()
    val questions = loadPosquestions.map { case (assumptions, goal) => ProofContext(goal, assumptions.map(s => Assumption(s))) }.toList
    var meanStepCount = 0.0
    questions.zipWithIndex.foreach {
      case (q, i) =>
        println("Problem " + (i + 1))
        val results = solver.prove(q).collect { case s:Success => s }
        Assert.assertFalse(s"Failed on problem $i; ${questions(i)}", results.isEmpty)
        val proof = results.head.proof
        ProofUtils.prettyPrint(proof);
        val proofSteps = ProofUtils.countSteps(proof)
        println(s"Found a proof with $proofSteps steps of inference after ${trace.stepCount} total attempted steps")
        println("--------------")
        meanStepCount += (trace.stepCount - meanStepCount) / (i+1)
      case _ => Unit
    }
    
    println(s"Average step count: ${Math.round(meanStepCount)}")
  }
  
  lazy val question: Parser[(List[Sentence], Sentence)] = (
    (CorePLProofParser.list <~ string("?-"), CorePLProofParser.sentence <~ char('.')).mapN { (premises, conc) => (premises, conc) }
  )
  
  private def loadPosquestions = {
    val res = ClassLoader.getSystemClassLoader.getResourceAsStream("asset")
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
      IfIntroduction, IfElimination,
      RestrictedDilemma))
}
