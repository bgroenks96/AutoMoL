package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.proof.types.Proof
import scala.collection.JavaConversions

object ProofTraverser {
  final def preOrderTraversal(proof: Proof): Seq[Proof] = children(proof) match {
    case Nil => Seq(proof)
    case childNodes => proof +: childNodes.flatMap(preOrderTraversal)
  }
  
  private def children(proof: Proof) = proof.conclusion match {
    case Some(conc) if conc.premiseCount > 0 => Seq[Proof](conc.major) ++ conc.minors
    case _ => Nil
  }
}
