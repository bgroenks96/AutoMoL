package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.Relation

sealed abstract class InferenceResult
case class CompleteResult(val conclusions: ObjectRelation*) extends InferenceResult
case class IncompleteResult(val requisites: InferenceRequisite*) extends InferenceResult 
case class NullResult() extends InferenceResult
