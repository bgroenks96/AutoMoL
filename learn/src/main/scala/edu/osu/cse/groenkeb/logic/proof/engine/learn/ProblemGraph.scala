package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic.proof.ProofContext

object ProblemGraph {
  def apply(context: ProofContext): ProblemGraph = {
    val (goalGraph, goalNode) = SentenceGraph.from(context.goal)
    val (premGraphs, premNodes) = context.available.toSeq.map(p => SentenceGraph.from(p.sentence)).unzip
    val mergedGraph = ((goalGraph +: premGraphs).reduce[SentenceGraph]{ case (g1, g2) => g1 ++ g2 })
    ProblemGraph(mergedGraph, goalNode, premNodes)
  }
}

case class ProblemGraph(graph: SentenceGraph, goal: GraphNode, assumptions: Seq[GraphNode])
