package edu.osu.cse.groenkeb.logic.engine.learn.repr

import org.junit.Test

import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.proof.ProofContext
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._

import scala.collection.immutable.Seq
import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemGraph
import edu.osu.cse.groenkeb.logic.proof.engine.learn.repr.DeepGraphEmbedding
import edu.osu.cse.groenkeb.logic.proof.engine.learn.repr.SpectralGraphEmbedding

class EmbeddingTests {
//  @Test
//  def testEmbedProblemBasic() {
//    implicit val rules = standardRules
//    val a = Sentences.atom("a")
//    val context = ProofContext(a, Seq(Assumption(a)))
//    val graph = ProblemGraph(context)
//    println(graph.graph.adj)
//    val embeddingModel = new DeepGraphEmbedding()
//    embeddingModel.embed(graph, steps=1)
//  }
  
  @Test
  def testEmbedProblemBasic() {
    implicit val rules = standardRules
    val a = Sentences.atom("a")
    val context = ProofContext(a, Seq(Assumption(a)))
    val graph = ProblemGraph(context)
    println(graph.graph.adj)
    println(SpectralGraphEmbedding.embed(graph.graph, k=2))
  }
  
  private def standardRules =
    RuleSet(Seq[Rule](
      NegationIntroduction, NegationElimination,
      AndIntroduction, AndElimination,
      OrIntroduction, OrElimination,
      IfIntroduction, IfElimination))
}