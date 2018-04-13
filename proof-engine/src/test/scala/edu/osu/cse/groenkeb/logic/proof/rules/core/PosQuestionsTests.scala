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
    val solver = new ProofSolver()
    val rules = standardRules
    val questions = loadPosquestions.toList
    val proofs = for {
      (assumptions, goal) <- questions
    } yield {
      (solver.prove(ProofContext(goal, rules, assume(assumptions:_*))).collect({ case r:Success => r }), trace.stepCount)
    }
    
    var meanStepCount = 0.0
    proofs.zipWithIndex.foreach {
      case ((results, steps), i) =>
        println("Problem " + (i + 1))
        Assert.assertFalse(s"Failed on problem $i; ${questions(i)}", results.isEmpty)
        ProofUtils.prettyPrint(results.head.proof);
        println(s"Finished after $steps attempted steps")
        println("--------------")
        meanStepCount += (steps - meanStepCount) / (i+1)
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
        println(q)
        Assert.assertFalse(solver.prove(q).filter(r => r.isInstanceOf[Success]).isEmpty)
        println(s"Finished after ${trace.stepCount} attempted steps")
        meanStepCount += (trace.stepCount - meanStepCount) / (i+1)
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
      IfIntroduction, IfElimination))
}
