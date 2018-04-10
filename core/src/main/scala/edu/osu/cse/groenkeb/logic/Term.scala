package edu.osu.cse.groenkeb.logic

final case class Term(name: String) {
  require(!name.isEmpty(), "term name must not be empty")
  require(name.forall { c => c.isLetterOrDigit }, "term name must be alpha-numeric")
  override def toString = name
}
