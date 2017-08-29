package edu.osu.cse.groenkeb.logic.test

import edu.osu.cse.groenkeb.logic.parse.DefaultPropOpMatcher
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.types._
import edu.osu.cse.groenkeb.logic.proof.rules._

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

  def run(context: ProofContext) {
    propSolver.prove(context).filter { x => x.isInstanceOf[Success] }.head match {
      case Success(proof, context, _) => {
        println("Success")
        ProofUtils.prettyPrint(proof)
      }
      case Failure(nullProof, _) => {
        println("Failure")
        println(String.format("No proof of %s given premises [%s]", context.goal, context.premises))
      }
    }
  }

  def exampleAndElim {
    val proofContext = ProofContext(sentenceA, rules, Premises.proud(complexSentence))
    run(proofContext)
  }
  
  def exapmleAndIntro {
    val proofContext = ProofContext(sentenceABC, rules, Premises.proud(sentenceA, sentenceB, sentenceC))
    run(proofContext)
  }
  
  def exampleAndElimWithInto {
    val proofContext = ProofContext(sentenceBD, rules, Premises.proud(sentenceD, sentenceABC))
    run(proofContext)
  }
}