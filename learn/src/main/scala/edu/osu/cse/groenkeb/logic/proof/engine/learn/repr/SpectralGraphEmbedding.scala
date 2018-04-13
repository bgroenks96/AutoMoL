package edu.osu.cse.groenkeb.logic.proof.engine.learn.repr

import smile.clustering.SpectralClustering
import smile.clustering.specc

import edu.osu.cse.groenkeb.logic.proof.engine.learn.ProblemGraph
import edu.osu.cse.groenkeb.logic.proof.engine.learn.SentenceGraph
import botkop.numsca.Tensor

object SpectralGraphEmbedding {
  def embed(graph: SentenceGraph, k: Int): Tensor = {
    val adjMatrix = toAdjMatrix(graph)
    println(adjMatrix.map(row => row.mkString(" ")).mkString("\n"))
    val clustering = specc(adjMatrix, k)
    Tensor(clustering.getClusterLabel.map(i => i.toDouble))
  }
  
  private def toAdjMatrix(graph: SentenceGraph) = {
    val N = graph.size
    val adjMat = Array.ofDim[Array[Double]](N)
    for (i <- 0 until N) {
      adjMat(i) = Array.ofDim(N)
    }
    val nodesWithIndex = graph.nodes.zipWithIndex
    val nodeMapping = nodesWithIndex.map { case (n, i) => n -> i }.toMap
    for ((n, i) <- nodesWithIndex) {
      for (a <- graph.adjIn(n) ++ graph.adjOut(n)) {
        val ind = nodeMapping(a)
        adjMat(i)(ind) += 1.0
        adjMat(ind)(i) += 1.0
      }
    }
    adjMat
  }
}