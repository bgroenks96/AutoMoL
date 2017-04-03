package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.Proof

object ProofUtils {
  def prettyPrint(proof: Proof) {
    var traverser = new ProofTraverser()
    var itr = traverser.preOrderTraversal(proof).iterator()
    var childCount = 0
    var nextChildCount = 0
    var prefix = ""
    while (itr.hasNext()) {
      var proof = itr.next()
      var conclusion = proof.conclusion.get
      var premiseCount = conclusion.args.prems.length
      nextChildCount += premiseCount      
      println(String.format("%s%s %s %s\n", prefix, conclusion.sentence.toString(), conclusion.rule.toString(), premiseCount.toString()))
      if (childCount > 0) {
        childCount -= 1
      } else {
        prefix = prefix + "  "
        childCount = nextChildCount - 1
        nextChildCount = 0
      }
    }
  }
}