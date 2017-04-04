package edu.osu.cse.groenkeb.logic.test

import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.PropSolver
import edu.osu.cse.groenkeb.logic.proof.NaiveProofStrategy
import edu.osu.cse.groenkeb.logic.proof.ProofUtils
import edu.osu.cse.groenkeb.logic.proof.Success
import edu.osu.cse.groenkeb.logic.proof.Failure
import edu.osu.cse.groenkeb.logic.proof.types.Premises

object Main extends App {
  implicit val matcher = new DefaultPropOpMatcher()
  implicit val proofStrategy = NaiveProofStrategy()
  val parser = new SentenceParser()
  val sentenceA = parser.parse("A")
  val sentenceB = parser.parse("B")
  val sentenceC = parser.parse("C")
  val sentenceD = parser.parse("D")
  val sentenceABC = parser.parse("and C (and A B)")
  val complexSentence = parser.parse("and (or D A) (and (and A B) (and C B))")
  val rules = RuleSet(Seq(AndIntroductionRule(), AndEliminationRule()))
  //implicit val proofContext = ProofContext(sentenceD, Premises.proud(complexSentence), rules)
  implicit val proofContext = ProofContext(sentenceABC, Premises.proud(sentenceA, sentenceB, sentenceC), rules)
  val propSolver = new PropSolver()
  
  propSolver.proof match {
    case Success(proof, _) => {
      println("Success")
      ProofUtils.prettyPrint(proof)
    }
    case Failure(nullProof, _) => {
      println("Failure")
      println(String.format("No proof of %s given premises [%s]", proofContext.goal, nullProof.prems.mkString(", ")))
    }
  }
  
  
  //val complexSentence = parser.parse("(A and (not (C or B)))", Notation("infix"))
  //println(complexSentence)
}