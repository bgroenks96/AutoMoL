package edu.osu.cse.groenkeb.logic.proof.rules

sealed abstract class RuleParity(val inverse: Rule)
case class Elimination(inv: Rule) extends RuleParity(inv)
case class Introduction(inv: Rule) extends RuleParity(inv)
case class None(self: Rule) extends RuleParity(self)
