package edu.osu.cse.groenkeb.logic.proof.engine.learn

import edu.osu.cse.groenkeb.logic.proof.ProofContext

object ProblemGraph {
  def apply(context: ProofContext): ProblemGraph = {
    val rootNode = RootNode(context.goal, context.available.toSeq.map(p => p.sentence))
    val (goalGraph, goalNode) = SentenceGraph(context.goal)
    val (premGraphs, premNodes) = context.available.toSeq.map(p => SentenceGraph(p.sentence)).unzip
    val rootGraph = SentenceGraph(Map[GraphNode, Adjacency](rootNode -> Adjacency(Nil, goalNode +: premNodes)))
    val mergedGraph = rootGraph ++ ((goalGraph +: premGraphs).reduce[SentenceGraph]{ case (g1, g2) => g1 ++ g2 })
    ProblemGraph(mergedGraph, goalNode, premNodes)
  }
}

case class ProblemGraph(graph: SentenceGraph, goal: GraphNode, assumptions: Seq[GraphNode])
