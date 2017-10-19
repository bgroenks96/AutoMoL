package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.Assumption
import edu.osu.cse.groenkeb.logic.proof.Proof
import edu.osu.cse.groenkeb.logic.Sentence

sealed abstract class Discharge(val assumptions: Assumption*) {
  /**
   * Returns either a collection of assumptions discharged by the given proof, or None if
   * the given proof fails to satisfy the requirements for discharge.
   */
  def discharge(proof: Proof): Option[Seq[Assumption]]
}
/**
 * A collection of assumptions that ALL must be used and discharged by a proof.
 */
case class Required(assmp: Assumption*) extends Discharge(assmp:_*) {
  def discharge(proof: Proof) = if (assmp forall { a => proof.premises exists { p => p.matches(a) }}) Some(assmp) else None
}
/**
 * A collection of assumptions that may optionally be used and discharged by a proof.
 */
case class Vacuous(assmp: Assumption*) extends Discharge(assmp:_*) {
  def discharge(proof: Proof) = Some(assmp filter { a => proof.premises exists { p => p.matches(a) }})
}
/**
 * A collection of assumptions where at least one must be used and discharged by a proof.
 */
case class Variate(assmp: Assumption*) extends Discharge(assmp:_*) {
  def discharge(proof: Proof) = assmp.filter { a => proof.premises exists { p => p.matches(a) }} match {
    case Nil => None
    case any => Some(any)
  }
}

