package edu.osu.cse.groenkeb.logic.proof

sealed abstract class Binding {
  def matches(b: Binding): Boolean
}
final case class IntBinding(id: Int) extends Binding {
  override def matches(b: Binding) = b match {
    case IntBinding(oid) => id == oid
    case _ => false
  }
  
  override def toString = id.toString()
}
final case class GroupBinding(bindings: Binding*) extends Binding {
  override def matches(b: Binding) = b match {
    case i@IntBinding(_) => bindings.exists { c => c.matches(i) }
    case GroupBinding(children@_*) => children.forall { c => bindings.exists { b => b.matches(c) } }
    case GlobalBinding => false
  }
    
  override def toString = bindings.mkString(",")
}
final case object GlobalBinding extends Binding {
  override def matches(b: Binding) = b == GlobalBinding
  
  override def toString = "global"
}
