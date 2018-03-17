package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.ExistentialQuantifier
import edu.osu.cse.groenkeb.logic.UniversalQuantifier

class DefaultPropOpMatcher() extends OperatorMatcher {
  def opFor(str: String): Operator = str match {
    case "and" => And
    case "or" => Or
    case "not" => Not
    case "if" => If
    case _ => NullOp
  }
  
  def nameFor(op: Operator): String = op match {
    case And => "and"
    case Or => "or"
    case Not => "not"
    case If => "if"
    case _ => ""
  }
}

class DefaultFirstOrderOpMatcher extends DefaultPropOpMatcher {
  val UQPattern = "(U)(:)?([a-z])".r
  val EQPattern = "(E)(:)?([a-z])".r
  
  override def opFor(str: String): Operator = str match {
    case UQPattern(_,_,x) => UniversalQuantifier(Term(x))
    case EQPattern(_,_,x) => ExistentialQuantifier(Term(x))
    case _ => super.opFor(str)
  }
  
  override def nameFor(op: Operator): String = op match {
    case ExistentialQuantifier(x) => "E:" + x.name
    case UniversalQuantifier(x) => "U:" + x.name
    case _ => super.nameFor(op)
  }
}
