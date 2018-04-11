package edu.osu.cse.groenkeb.logic

case class Atom(val predicate: Predicate, val terms: Term*) {
  def matches(atom: Atom) = atom != null && predicate.matches(atom.predicate) && terms.equals(atom.terms)
  
  def toRelation = ObjectRelation(predicate, terms:_*)
  
  def substitute(orig: Term, sub: Term) = Atom(predicate, terms map { t => t match {
    case `orig` => sub
    case _ => t
  }}:_*)
  
  override def toString = terms match {
    case Nil => predicate.toString()
    case terms => String.format("%s[%s]", predicate.toString(), terms.mkString("."))
  }
}

object Atom {
  def parse(str: String) = {
    val propPattern = "([A-z0-9_]+)".r
    val objPattern = "([A-z0-9_]+)\\[([A-z0-9,_]+)*\\]".r
    val idpred = IdentityPredicate.name
    str match {
      case objPattern(`idpred`, argstr) => Atom(IdentityPredicate(), argstr.split(",").map { s => Term(s) }:_*)
      case objPattern(pred, argstr) => Atom(NamedPredicate(pred), argstr.split(",").map { s => Term(s) }:_*)
      case propPattern(pred) => Atom(NamedPredicate(pred))
      case _ => throw new Exception("invalid definition of atom: " + str)
    }
  }
}