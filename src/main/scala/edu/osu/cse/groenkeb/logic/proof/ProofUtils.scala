package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.proof.rules.NullRule
import edu.osu.cse.groenkeb.logic.proof.types.NullProof

object ProofUtils {
  def prettyPrint(proof: Proof) {
    var traverser = new ProofTraverser()
    var itr = traverser.preOrderTraversal(proof).iterator()
    prettyPrint(itr, "")
  }
  
  private def prettyPrint(itr: java.util.Iterator[Proof], prefix: String) {
    if (!itr.hasNext()) return
    var proof = itr.next()
    if (proof.isInstanceOf[NullProof]) {
      println("No proof.")
      return;
    }
    
    var conclusion = proof.conclusion.get
    if (conclusion.rule.isInstanceOf[NullRule]) return;
    println(String.format("%s %s %s {%s}\n", prefix, conclusion.sentence, conclusion.rule, proof.premises.mkString(", ")))
    conclusion.args.prems foreach { p => prettyPrint(itr, prefix + "   ") }
  }
}