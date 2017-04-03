package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.types.Assumption

sealed abstract class Discharge(val assumptions: Assumption*)
case class Required(assmp: Assumption*) extends Discharge(assmp:_*)
case class Vacuous(assmp: Assumption*) extends Discharge(assmp:_*)
case class Variate(assmp: Assumption*) extends Discharge(assmp:_*)
