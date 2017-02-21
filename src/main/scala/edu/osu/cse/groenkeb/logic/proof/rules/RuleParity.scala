package edu.osu.cse.groenkeb.logic.proof.rules

sealed abstract class RuleParity(inverse: Rule)
case class Elimination(inverse: Rule) extends RuleParity(inverse)
case class Introduction(inverse: Rule) extends RuleParity(inverse)
case class None(self: Rule) extends RuleParity(self)
