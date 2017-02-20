package edu.osu.cse.groenkeb.logic.proof.types

sealed abstract class Proof(val conclusion: Option[Conclusion], val premises: Seq[Premise])
case class CompleteProof(conc: Conclusion, prems: Seq[Premise]) extends Proof(Option.apply(conc), prems)
case class NullProof(prems: Seq[Premise]) extends Proof(Option.empty[Conclusion], prems)
