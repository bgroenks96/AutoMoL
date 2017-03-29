package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.types.Assumption

sealed abstract class Discharge
case class Required(assumption: Assumption) extends Discharge
case class Vacuous(assumption: Assumption) extends Discharge
case class Variate(discharges: Discharge*) extends Discharge
case class Composite(discharges: Discharge*) extends Discharge
