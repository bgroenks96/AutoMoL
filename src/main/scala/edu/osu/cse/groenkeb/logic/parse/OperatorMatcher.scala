package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

trait OperatorMatcher {
  def opFor(str: String): Operator
  def nameFor(op: Operator): String
}

case class DefaultOperatorMatcher() extends OperatorMatcher {
  def opFor(str: String): Operator = str match {
    case "and" => And()
    case "or" => Or()
    case "not" => Not()
    case _ => Null()
  }
  
  def nameFor(op: Operator): String = op match {
    case And() => "and"
    case Or() => "or"
    case Not() => "not"
    case _ => ""
  }
}