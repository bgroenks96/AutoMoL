package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

class DefaultPropOpMatcher() extends OperatorMatcher {
  def opFor(str: String): Operator = str match {
    case "and" => And()
    case "or" => Or()
    case "not" => Not()
    case "->" => Implies()
    case _ => NullOp()
  }
  
  def nameFor(op: Operator): String = op match {
    case And() => "and"
    case Or() => "or"
    case Not() => "not"
    case Implies() => "->"
    case _ => ""
  }
}

class DefaultFirstOrderOpMatcher extends DefaultPropOpMatcher {
  val UQPattern = "(U)(:)?([A-z]+)".r
  val EQPattern = "(E)(:)?([A-z]+)".r
  
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
