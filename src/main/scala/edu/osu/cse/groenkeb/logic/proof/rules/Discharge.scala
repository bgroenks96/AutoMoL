package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence

sealed abstract class Discharge
case class Required(sentence: Sentence) extends Discharge
case class Vacuous(sentence: Sentence) extends Discharge
case class Variate(discharges: Discharge*) extends Discharge
case class Composite(discharges: Discharge*) extends Discharge
case class Empty() extends Discharge
