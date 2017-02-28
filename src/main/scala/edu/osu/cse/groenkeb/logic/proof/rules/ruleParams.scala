package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.ObjectRelation

sealed abstract class RuleParam
case class AnyProof(val conc: ObjectRelation) extends RuleParam
case class RelevantProof(val conc: ObjectRelation, val from: Discharge) extends RuleParam

sealed abstract class RuleParams
case class EmptyParams() extends RuleParams
case class UnaryParams(val param0: RuleParam) extends RuleParams
case class BinaryParams(val param0: RuleParam, val param1: RuleParam) extends RuleParams
case class TernaryParams(val param0: RuleParam, val param1: RuleParam, val param2: RuleParam) extends RuleParams
