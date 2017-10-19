package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.proof.Proof

sealed abstract class InferenceResult
case class CompleteResult(val proof: Proof) extends InferenceResult
case class IncompleteResult(val params: RuleParams) extends InferenceResult
case class NullResult() extends InferenceResult
