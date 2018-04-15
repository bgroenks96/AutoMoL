package edu.osu.cse.groenkeb.logic.proof.engine.learn.util

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.dsl._
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.proof.engine.Trace
import edu.osu.cse.groenkeb.logic.proof.engine.CoreProofStrategy
import edu.osu.cse.groenkeb.logic.proof.engine.ProofSolver
import edu.osu.cse.groenkeb.logic.proof.engine.Success
import edu.osu.cse.groenkeb.logic.proof.rules.core._
import edu.osu.cse.groenkeb.logic.proof.rules._

import scala.collection.immutable.Seq
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import edu.osu.cse.groenkeb.logic.proof.Premise

object ProblemGenerator {
  def main(args: Array[String]) = {
    implicit val strategy = new CoreProofStrategy()
    implicit val trace = Trace()
    implicit val options = Seq(trace)
    val solver = new ProofSolver
    val predicates = Seq("F", "R", "Q").map(s => NamedPredicate(s))
    val domain = Domain(Term("a"), Term("b"), Term("c"))
    implicit val rules = standardRules
    val probs = generateProblems(predicates, domain, 3, 1, 1)
    var pcount = 0
    var rcount = 0
    var total = 0
    val N = 100000
    val sampleFreq = 0.00001f
    val iterator = probs.iterator
    val selected = ArrayBuffer[ProofContext]()
    while (iterator.hasNext && total < N) {
      if (Random.nextFloat() < sampleFreq) {
        val p = iterator.next()
        val presults = solver.prove(p).collect { case s:Success => s }
        if (!presults.isEmpty) {
          pcount += 1
          selected += p
        } else {
          val rp = ProofContext(Absurdity, p.available + Assumption(Not(p.goal)))
          val rresults = solver.prove(rp).collect { case s:Success => s }
          if (!rresults.isEmpty) {
            rcount += 1
            selected += rp
          }
        }
        total += 1
      }
    }
    
    println(s"Evaluated $N problems, found $pcount proofs and $rcount refutations")
    val outFileName = "autoprobs"
    dumpToFile(outFileName, selected)
  }
  
  def generateProblems(predicates: Seq[Predicate],
                       domain: Domain,
                       premiseCount: Int = 1,
                       maxPremiseDepth: Int = 1,
                       maxGoalDepth: Int = 1)(implicit rules: RuleSet): Stream[ProofContext] = {
    val problems = premiseCount match {
      case 0 => for {
        s <- generateSentences(maxGoalDepth, predicates, domain)
    } yield ProofContext(s)
      case i =>
        def next = generateProblems(predicates, domain, premiseCount - 1, maxPremiseDepth, maxGoalDepth)
        lazy val newProbs = for {
          s <- generateSentences(maxPremiseDepth, predicates, domain)
          p <- next
        } yield ProofContext(p.goal, p.available + Assumption(s))
        next #::: newProbs
    }
    problems.filter {
      p => (p.goal +: p.goal.decomposeRecursive).exists {
        case s if p.available.isEmpty => !s.hasRec(And) && !s.hasRec(Or) && !s.hasRec(Not)
        case s => p.available.exists {
          a => a.sentence.accessible(s)
        }
      } and !p.available.exists(a => a.matches(p.goal))
    }
  }
  
  def generateSentences(depth: Int, predicates: Seq[Predicate], domain: Domain): Stream[Sentence] = depth match {
    case 0 => for {
      p <- predicates.toStream
      t <- domain.terms.toStream
    } yield AtomicSentence(Atom(p, t))
    case i =>
      def next = generateSentences(depth - 1, predicates, domain)
      def conditionals = for {
        left <- next
        right <- next
        if (!left.matches(right))
      } yield If(left, right)
      def conjunctions = for {
        (left, i) <- next.zipWithIndex
        right <- next.iterator.drop(i+1).toStream
      } yield And(left, right)
      def disjunctions = for {
        (left, i) <- next.zipWithIndex
        right <- next.iterator.drop(i+1).toStream
      } yield Or(left, right)
      def negations = for {
        s <- next
      } yield Not(s)
      for {
        l <- next #::: conjunctions #::: disjunctions #::: negations #::: conditionals
      } yield l
      
  }
  
  def dumpToFile(fileName: String, problems: scala.Seq[ProofContext]) = {
    def sentToStr(sentence: Sentence): String = sentence match {
      case Absurdity => Absurdity.toString()
      case AtomicSentence(atom) => atom.toString()
      case BinarySentence(left, right, conn) => s"($conn ${sentToStr(left)} ${sentToStr(right)})"
      case UnarySentence(s, conn) => s"($conn ${sentToStr(s)})"
      case _ => ???
    }
    def premsToStr(prems: scala.Seq[Premise]) = s"[${prems.map(p => sentToStr(p.sentence)).mkString(",")}]"
    def question(prems: scala.Seq[Premise], goal: Sentence) = s"${premsToStr(prems)}?-${sentToStr(goal)}."
    val writer = new java.io.PrintWriter(fileName, java.nio.charset.StandardCharsets.UTF_8.name())
    try {
      for (p <- problems) {
        writer.println(question(p.available.toSeq, p.goal))
      }
    } finally {
      writer.close()
    }
  }
  
  private def standardRules =
    RuleSet(Seq[Rule](
      NegationIntroduction, NegationElimination,
      AndIntroduction, AndElimination,
      OrIntroduction, OrElimination,
      IfIntroduction, IfElimination))
}
