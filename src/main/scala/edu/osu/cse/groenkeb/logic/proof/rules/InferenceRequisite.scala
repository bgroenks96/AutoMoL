package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Relation
import edu.osu.cse.groenkeb.logic.MetaRelation

sealed abstract class InferenceRequisite
case class Require(relation: Relation) extends InferenceRequisite
case class Vacuous(relation: Relation) extends InferenceRequisite
case class Discrete(reqA: InferenceRequisite, reqB: InferenceRequisite) extends InferenceRequisite
case class Composite(requisites: InferenceRequisite*) extends InferenceRequisite
