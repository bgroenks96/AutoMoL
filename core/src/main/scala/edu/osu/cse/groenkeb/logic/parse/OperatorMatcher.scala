package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic.Operator

trait OperatorMatcher {
  def opFor(str: String): Operator
  def nameFor(op: Operator): String
}
