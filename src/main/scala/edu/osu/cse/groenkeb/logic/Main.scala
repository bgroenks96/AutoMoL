package edu.osu.cse.groenkeb.logic

import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.parse.DefaultOperatorMatcher
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer
import edu.osu.cse.groenkeb.logic.parse.Notation
import edu.osu.cse.groenkeb.logic.parse.SentenceParser
import edu.osu.cse.groenkeb.logic.proof.types.ProudPremise
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.PropSolver
import edu.osu.cse.groenkeb.logic.proof.DefaultPremiseSelector

object Main extends App {
  implicit val matcher = DefaultOperatorMatcher()
  val tokenizer = new NodeRecursiveTokenizer()
  val parser = new SentenceParser(tokenizer)
  val sentenceA = parser.parse("A")
  val sentenceB = parser.parse("B")
  val sentenceAandB = parser.parse("(and A B)")
  val rules = RuleSet(ReflexivityRule())
  val premise = ProudPremise(sentenceAandB)
  implicit val proofContext = ProofContext(List(premise), List(), rules)
  val propSolver = new PropSolver(new DefaultPremiseSelector())
  
  println(propSolver.prove(sentenceAandB))
  
  //val complexSentence = parser.parse("(A and (not (C or B)))", Notation("infix"))
  //println(complexSentence)
}