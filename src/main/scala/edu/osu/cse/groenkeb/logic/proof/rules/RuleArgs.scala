package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.types.Proof

sealed abstract class RuleArgs
case class EmptyArgs() extends RuleArgs
case class UnaryArgs(val major: Proof) extends RuleArgs
case class BinaryArgs(val major: Proof, val minor: Proof) extends RuleArgs
case class TernaryArgs(val major: Proof, val minor1: Proof, val minor2: Proof) extends RuleArgs
