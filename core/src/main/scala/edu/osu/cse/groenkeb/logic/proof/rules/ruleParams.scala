package edu.osu.cse.groenkeb.logic.proof.rules

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.Assumption

sealed abstract class RuleParam(val goal: Sentence)
/**
 * Parameter that requires any proof of 'c' from any set of premises.
 */
case class AnyProof(c: Sentence) extends RuleParam(c)
/**
 * Parameter that requires a relevant proof of 'c' from a set of premises that has at least those
 * specified by 'discharge' and omits any premises included in 'restrict'.
 */
case class RelevantProof(c: Sentence, val from: Discharge, val restrict: Assumption*) extends RuleParam(c)
/**
 * Parameter that requires the given sentence to "stand proud" as the major premise in the rule.
 */
case class EmptyProof(c: Sentence) extends RuleParam(c)

sealed abstract class RuleParams
case object EmptyParams extends RuleParams
case class UnaryParams(param0: RuleParam) extends RuleParams
case class BinaryParams(param0: RuleParam, param1: RuleParam) extends RuleParams
case class TernaryParams(param0: RuleParam, param1: RuleParam, param2: RuleParam) extends RuleParams
case class NParams(params: Seq[RuleParam]) extends RuleParams
case class OptionParams(paramSets: RuleParams*) extends RuleParams
