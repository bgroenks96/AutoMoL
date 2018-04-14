package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.rules._

object ProofUtils {
  def countSteps(proof: Proof): Int = proof match {
    case Proof(_,_, EmptyArgs,_,_) => 1
    case Proof(_,_, UnaryArgs(p),_,_) => 1 + countSteps(p)
    case Proof(_,_, BinaryArgs(p1, p2),_,_) => 1 + countSteps(p1) + countSteps(p2)
    case Proof(_,_, TernaryArgs(p1, p2, p3),_,_) => 1 + countSteps(p1) + countSteps(p2) + countSteps(p3)
    case Proof(_,_, NArgs(ps),_,_) => 1 + ps.map(p => countSteps(p)).sum
    case _ => ???
  }
  
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