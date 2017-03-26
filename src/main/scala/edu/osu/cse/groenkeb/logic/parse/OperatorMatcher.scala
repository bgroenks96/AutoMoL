package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

trait OperatorMatcher {
  def opFor(str: String): Operator
  def nameFor(op: Operator): String
}

case class DefaultOperatorMatcher() extends OperatorMatcher {
  val UQPattern = "(U)(:)?(.*)".r
  val EQPattern = "(E)(:)?(.*)".r
  def opFor(str: String): Operator = str match {
    case "and" => And()
    case "or" => Or()
    case "not" => Not()
    case ">>" => Implies()
    case UQPattern(_,_,x) => UniversalQuantifier(Term(x))
    case EQPattern(_,_,x) => ExistentialQuantifier(Term(x))
    case _ => NullOp()
  }
  
  def nameFor(op: Operator): String = op match {
    case And() => "and"
    case Or() => "or"
    case Not() => "not"
    case Implies() => ">>"
    case ExistentialQuantifier(x) => "E:" + x.name
    case UniversalQuantifier(x) => "U:" + x.name
    case _ => ""
  }
}