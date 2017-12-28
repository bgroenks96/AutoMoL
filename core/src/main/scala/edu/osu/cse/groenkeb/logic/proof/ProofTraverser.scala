package edu.osu.cse.groenkeb.logic.proof

import scala.collection.JavaConversions

object ProofTraverser {
  final def preOrderTraversal(proof: Proof): Seq[Proof] = children(proof) match {
    case Nil => Seq(proof)
    case childNodes => proof +: childNodes.flatMap(preOrderTraversal)
  }
  
  private def children(proof: Proof) = proof match {
    case Proof(s, rule, args, prems) if args.prems.length > 0 => args.prems
    case _ => Nil
  }
}
