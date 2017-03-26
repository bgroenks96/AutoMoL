package edu.osu.cse.groenkeb.logic

case class Term(val name: String) {
  require(!name.isEmpty(), "term name must not be empty")
  require(name.forall { c => c.isLetterOrDigit }, "term name must be alpha-numeric")
  override def toString = name
}