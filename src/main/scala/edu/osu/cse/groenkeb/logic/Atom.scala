package edu.osu.cse.groenkeb.logic

case class Atom(val name: String) {
  def matches(atom: Atom) = atom != null && name.equals(atom.name)
  
  override def toString = name
}

object Atom {
  final val absurdity = new Atom("!")
}