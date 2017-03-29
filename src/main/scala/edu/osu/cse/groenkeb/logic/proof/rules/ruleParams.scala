package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.Assumption

sealed abstract class RuleParam
/**
 * Parameter that requires any proof of 'conc' from any set of premises.
 */
case class AnyProof(val conc: Sentence) extends RuleParam
/**
 * Parameter that requires a relevant proof of 'conc' from a set of premises that has at least those
 * specified by 'discharge' and omits any premises included in 'restrict'.
 */
case class RelevantProof(val conc: Sentence, val from: Discharge, val restrict: Seq[Assumption] = Nil) extends RuleParam

sealed abstract class RuleParams
case class EmptyParams() extends RuleParams
case class UnaryParams(val param0: RuleParam) extends RuleParams
case class BinaryParams(val param0: RuleParam, val param1: RuleParam) extends RuleParams
case class TernaryParams(val param0: RuleParam, val param1: RuleParam, val param2: RuleParam) extends RuleParams
