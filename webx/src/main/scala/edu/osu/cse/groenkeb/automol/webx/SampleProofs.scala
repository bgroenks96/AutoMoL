package edu.osu.cse.groenkeb.automol.webx

import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._

object SampleProofs {
  val Id = ProudPremise(atom("P")).proof
  
  private def atom(str: String) = AtomicSentence(Atom.parse(str))
}