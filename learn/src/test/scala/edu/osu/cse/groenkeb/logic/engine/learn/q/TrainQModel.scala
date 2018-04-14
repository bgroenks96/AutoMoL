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
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemState

object TrainQModel {
  val gamma = 0.9
  val policy = new EpsilonGreedy(0.0, decay=0.0)
  val features = Seq(basicRuleFilter, ruleOrdering, atomicAccessibility, majorComplexityScore, shortestPathToGoal)
  val model = new LinearQModel(features, gamma)
  val alphaDecay = 0.0
  val alphaMin = 0.0
  val numEpochs = 2
  val numSplits = 3 // 3-fold cross validation
  implicit val trace = Trace()
  implicit val options = Seq(trace)
  implicit val rules = standardRules

  def main(args: Array[String]) = {
    implicit val trace = Trace()
    implicit val options = Seq(trace)
    implicit val rules = standardRules
    val questions = loadData.map { case (assumptions, goal) => ProofContext(goal, assumptions.map(s => Assumption(s))) }.toIndexedSeq
    trainWith(questions)
  }
  
  private def trainWith(questions: IndexedSeq[ProofContext]) {
    val baselineSolver = new ProofSolver()(new CoreProofStrategy(), options)
    val partitions = splitForCrossValidation(questions, k=numSplits)
    for (i <- 1 to numEpochs) {
      println("Starting epoch " + i)
      var expectedReward = 0.0
      var nUpdates = 0
      val updateExpectedReward = (s: ProblemState, r: Double) => {
        nUpdates += 1
        expectedReward += (r - expectedReward) / nUpdates
      }
      implicit val strategy = new QLearningStrategy(model, policy, alpha=0.01, alphaDecay=alphaDecay, alphaMin=alphaMin, updateCallback=updateExpectedReward)
      val solver = new ProofSolver
      var avgStepCountTrain = 0.0
      var avgStepCountVal = 0.0
      var avgStepCountBase = 0.0
      var avgEffScoreTrain = 0.0
      var avgEffScoreVal = 0.0
      var avgEffScoreBase = 0.0
      for (j <- 0 until numSplits) {
        val valSet = partitions(j)
        val trainSet = Array.range(0, numSplits).filter(i => i != j).flatMap(i => partitions(i))
        model.setMode(QModel.TrainMode)
        println(s"begin training pass ${j+1} for epoch $i")
        trainSet.zipWithIndex.foreach {
          case (prob, i) => {
            //println(s"Problem $i : $prob")
            val results = solver.prove(prob).collect { case s:Success => s.proof }
            Assert.assertFalse(results.isEmpty)
            val proof = results.head
            val steps = ProofUtils.countSteps(proof)
            //println(s"Found a proof with $steps steps of inference after ${trace.stepCount} total attempted steps")
            val effScore = steps / trace.stepCount.toDouble
            //println("Efficiency score: " + effScore)
            avgEffScoreTrain += (effScore - avgEffScoreTrain) / (j*trainSet.length+i+1)
            avgStepCountTrain += (trace.stepCount - avgStepCountTrain) / (j*trainSet.length+i+1)
          }
        }
        println("average efficiency score (train): " + avgEffScoreTrain)
        println("average total step count (train): " + avgStepCountTrain)
        println(s"expected reward ($nUpdates updates): " + expectedReward)
        expectedReward = 0.0
        nUpdates = 0
        println(s"begin validation pass ${j+1} for epoch $i")
        model.setMode(QModel.TestMode)
        valSet.zipWithIndex.foreach {
          case (prob, i) => {
            //println(s"Problem $i : $prob")
            val results = solver.prove(prob).collect { case s:Success => s.proof }
            Assert.assertFalse(results.isEmpty)
            val proof = results.head
            val steps = ProofUtils.countSteps(proof)
            //println(s"Found a proof with $steps steps of inference after ${trace.stepCount} total attempted steps")
            val effScore = steps / trace.stepCount.toDouble
            //println("Efficiency score: " + effScore)
            avgEffScoreVal += (effScore - avgEffScoreVal) / (j*valSet.length+i+1)
            avgStepCountVal += (trace.stepCount - avgStepCountVal) / (j*valSet.length+i+1)
          }
        }
        println("average efficiency score (val): " + avgEffScoreVal)
        println("average total step count (val): " + avgStepCountVal)
        println(s"expected reward: " + expectedReward)
        expectedReward = 0.0
        nUpdates = 0
        println(s"begin baseline test ${j+1} for epoch $i")
        valSet.zipWithIndex.foreach {
          case (prob, i) => {
            //println(s"Problem $i : $prob")
            val results = baselineSolver.prove(prob).collect { case s:Success => s.proof }
            Assert.assertFalse(results.isEmpty)
            val proof = results.head
            val steps = ProofUtils.countSteps(proof)
            //println(s"Found a proof with $steps steps of inference after ${trace.stepCount} total attempted steps")
            val effScore = steps / trace.stepCount.toDouble
            //println("Efficiency score: " + effScore)
            avgEffScoreBase += (effScore - avgEffScoreBase) / (j*valSet.length+i+1)
            avgStepCountBase += (trace.stepCount - avgStepCountBase) / (j*valSet.length+i+1)
          }
        }
        println("average total step count (base): " + avgStepCountBase)
      }
      println("Average efficiency score (train): " + avgEffScoreTrain)
      println("Average efficiency score (val): " + avgEffScoreVal)
      println("Average efficiency score (val w/ baseline): " + avgEffScoreBase)
      println("average total step count (train): " + avgStepCountTrain)
      println("average total step count (val): " + avgStepCountVal)
      println("average total step count (val w/ baseline): " + avgStepCountBase)
      println("current learning rate: " + strategy.alpha)
      println("weights: " + model.weights.mkString(" "))
    }
    println("done")
    println("final model weights: " + model.weights)
  }
  
  private def splitForCrossValidation(train: IndexedSeq[ProofContext], k: Int = 3) = {
    val partitionSize = train.size / k
    val partitions = Array.ofDim[IndexedSeq[ProofContext]](k)
    for (i <- 0 until k) {
      partitions(i) = train.slice(i*partitionSize, (i+1)*partitionSize)
    }
    partitions
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
  
  private def loadData = {
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
