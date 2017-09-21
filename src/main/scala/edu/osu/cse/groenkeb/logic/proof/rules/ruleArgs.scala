package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.types.Proof

sealed abstract class RuleArgs(val prems: Proof*)
case class EmptyArgs() extends RuleArgs
case class UnaryArgs(major: Proof) extends RuleArgs(major)
case class BinaryArgs(major: Proof, minor: Proof) extends RuleArgs(major, minor)
case class TernaryArgs(major: Proof, minor1: Proof, minor2: Proof) extends RuleArgs(major, minor1, minor2)
case class NArgs(proofs: Seq[Proof]) extends RuleArgs(proofs:_*)
