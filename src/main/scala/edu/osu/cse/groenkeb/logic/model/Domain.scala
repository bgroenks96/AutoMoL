package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Atom

case class Domain(val atoms: Seq[Atom]) {
  def :+(atom: Atom) = withMember(atom)
  def withMember(atom: Atom) = if (lacks(atom)) Domain(atoms :+ atom); else this
  def has(atom: Atom) = atoms.contains(atom)
  def lacks(atom: Atom) = !has(atom)
}