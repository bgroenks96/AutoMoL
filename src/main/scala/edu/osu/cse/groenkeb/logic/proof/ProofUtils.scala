package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.Proof

object ProofUtils {
  def prettyPrint(proof: Proof) {
    var traverser = new ProofTraverser()
    var itr = traverser.breadthFirstTraversal(proof).iterator()
    while (itr.hasNext())
    {
      var next = itr.next()
      
    }
  }
}