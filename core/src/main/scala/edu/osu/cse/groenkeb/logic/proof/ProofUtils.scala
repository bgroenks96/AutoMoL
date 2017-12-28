package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.rules.NullRule

object ProofUtils {
  def prettyPrint(proof: Proof) {
    var itr = ProofTraverser.preOrderTraversal(proof).iterator
    prettyPrint(itr, "")
  }
  
  private def prettyPrint(itr: Iterator[Proof], prefix: String) {
    if (!itr.hasNext) return
    var proof = itr.next()
    
    if (proof.rule == NullRule) return;
    println(String.format("%s %s %s [%s] { %s }\n", prefix, proof.sentence, proof.rule, proof.binding, proof.undischarged.mkString(", ")))
    proof.args.prems foreach { p => prettyPrint(itr, prefix + "   ") }
  }
}