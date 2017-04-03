package edu.osu.cse.groenkeb.logic.proof

import com.google.common.collect.TreeTraverser
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import scala.collection.JavaConversions

class ProofTraverser extends TreeTraverser[Proof] {
  override def children(proof: Proof) = proof.conclusion match {
    case Some(conc) if conc.premiseCount > 0 => JavaConversions.asJavaIterable(Seq[Proof](conc.major) ++ conc.minors)
    case _ => JavaConversions.asJavaIterable(Seq())
  }
}