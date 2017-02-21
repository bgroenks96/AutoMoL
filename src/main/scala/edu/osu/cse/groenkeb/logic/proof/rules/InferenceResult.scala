package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Relation
import edu.osu.cse.groenkeb.logic.proof.types.Proof
import edu.osu.cse.groenkeb.logic.Turnstile

sealed abstract class InferenceResult
case class CompleteResult(val proof: Proof) extends InferenceResult
case class IncompleteResult(val required: Turnstile*) extends InferenceResult 
case class NullResult() extends InferenceResult
