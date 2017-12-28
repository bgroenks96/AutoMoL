package edu.osu.cse.groenkeb.logic.utils

object Only {
  /**
   * Extracts a singleton Seq from the Set, returns None if the Set is empty or has more than one element.
   */
  def unapply[T](set: Set[T]): Option[T] = set.toSeq match {
    case Seq(elem) => Some(elem)
    case _ => None
  }
}
