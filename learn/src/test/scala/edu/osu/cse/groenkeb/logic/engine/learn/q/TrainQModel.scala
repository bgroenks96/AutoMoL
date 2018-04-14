package edu.osu.cse.groenkeb.logic.engine.learn.q

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.parse.ParserException
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.QLearningStrategy
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer
import edu.osu.cse.groenkeb.logic.proof.engine.Trace
import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.Features._
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.LinearQModel
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.EpsilonGreedy

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.immutable.Seq
import java.io.BufferedReader
import edu.osu.cse.groenkeb.logic.parse.corepl.CorePLProofParser
import java.io.InputStreamReader
import org.junit.Assert

import atto._
import atto.Atto._
import atto.ParseResult._
import cats.implicits._
import scala.util.Random
import edu.osu.cse.groenkeb.logic.proof.engine.learn.q.QModel

object TrainQModel {
  val gamma = 0.9
  val policy = new EpsilonGreedy(0.0, decay=0.0)
  val features = Seq(introRuleFilter, ruleOrdering, accessibility)
  val model = new LinearQModel(features, gamma)
  val alphaDecay = 0.0
  val alphaMin = 1.0E-5
  implicit val strategy = new QLearningStrategy(model, policy, alpha=0.00f, alphaDecay=alphaDecay, alphaMin=alphaMin)
  implicit val trace = Trace()
  implicit val options = Seq(trace)
  implicit val parser = new SentenceParser(new NodeRecursiveTokenizer())(new DefaultPropOpMatcher())  

  def main(args: Array[String]) = {
    //Random.setSeed(1)
    implicit val trace = Trace()
    implicit val options = Seq(trace)
    implicit val rules = standardRules
    val solver = new ProofSolver()
    val questions = loadPosquestions.map { case (assumptions, goal) => ProofContext(goal, assumptions.map(s => Assumption(s))) }.toList
    val (trainSet, valSet) = splitForValidation(questions)
    for (i <- 1 to 10) {
      model.setMode(QModel.TrainMode)
      var trainStepCount = 0.0
      trainSet.foreach{ case (q, i) => {
        println(s"Problem $i (train): $q")
        Assert.assertFalse(solver.prove(q).collect { case s:Success => s.proof }.isEmpty)
        trainStepCount += trace.stepCount
      }}
      model.setMode(QModel.TestMode)
      var valStepCount = 0.0
      valSet.foreach{ case (q, i) => {
        println(s"Problem $i (validation): $q")
        Assert.assertFalse(solver.prove(q).collect { case s:Success => s.proof }.isEmpty)
        valStepCount += trace.stepCount
      }}
      println(s"Iteration $i complete: AvgSteps train: ${trainStepCount/trainSet.length} val: ${valStepCount/valSet.length}  combined: ${(trainStepCount + valStepCount)/questions.length}")
      println("current learning rate: " + strategy.alpha)
      strategy.alpha = strategy.alpha - 0.0005
    }
    println("done")
  }
  
  private def splitForValidation(train: Seq[ProofContext], split: Float = 0.2f) = {
    val arange = Array.range(0, train.length).toList
    val valCount = (split*train.length).toInt
    val randomIndices = Random.shuffle(arange).take(valCount)
    val (vpart, tpart) = train.zipWithIndex.partition { case (c, i) => randomIndices.contains(i) }
    (tpart, vpart)
  }
  
  lazy val question: Parser[(List[Sentence], Sentence)] = (
    (CorePLProofParser.list <~ string("?-"), CorePLProofParser.sentence <~ char('.')).mapN { (premises, conc) => (premises, conc) }
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
  
  private def standardRules =
    RuleSet(Seq[Rule](
      NegationIntroduction, NegationElimination,
      AndIntroduction, AndElimination,
      OrIntroduction, OrElimination,
      IfIntroduction, IfElimination))
}
