package edu.osu.cse.groenkeb.logic.test

import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.ProofSolver
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
  val sentenceBD = parser.parse("and B D")
  val sentenceABC = parser.parse("and (and A B) C")
  val complexSentence = parser.parse("and (or D A) (and (and A B) (and C B))")
  val rules = RuleSet(Seq(AndIntroductionRule(), AndEliminationRule()))
  val propSolver = new ProofSolver()
 
  exapmleAndIntro
  println("--------")
  exampleAndElim
  println("--------")
  exampleAndElimWithInto
  println("--------")

  def run(implicit context: ProofContext) {
    propSolver.proof match {
      case Success(proof, _) => {
        println("Success")
        ProofUtils.prettyPrint(proof)
      }
      case Failure(nullProof, _) => {
        println("Failure")
        println(String.format("No proof of %s given premises [%s]", context.goal, nullProof.prems.mkString(", ")))
      }
    }
  }

  def exampleAndElim {
    implicit val proofContext = ProofContext(sentenceA, Premises.proud(complexSentence), rules)
    run
  }
  
  def exapmleAndIntro {
    implicit val proofContext = ProofContext(sentenceABC, Premises.proud(sentenceA, sentenceB, sentenceC), rules)
    run
  }
  
  def exampleAndElimWithInto {
    implicit val proofContext = ProofContext(sentenceBD, Premises.proud(sentenceD, sentenceABC), rules)
    run
  }
}