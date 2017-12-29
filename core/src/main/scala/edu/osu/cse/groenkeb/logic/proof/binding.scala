package edu.osu.cse.groenkeb.logic.proof

sealed abstract class Binding
final case class IntBinding(id: Int) extends Binding {
  override def toString = id.toString()
}
final case class NameBinding(name: String) extends Binding {
  override def toString = name
}
final case object GlobalBinding extends Binding {
  override def toString = "<global>"
}
