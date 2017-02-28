package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

trait OperatorMatcher {
  def opFor(str: String): Operator[_]
  def nameFor(op: Operator[_]): String
}

case class DefaultOperatorMatcher() extends OperatorMatcher {
  def opFor(str: String): Operator[_] = str match {
    case "and" => AndOp()
    case "or" => OrOp()
    case "not" => NotOp()
    case _ => NullOp()
  }
  
  def nameFor(op: Operator[_]): String = op match {
    case AndOp() => "and"
    case OrOp() => "or"
    case NotOp() => "not"
    case _ => ""
  }
}