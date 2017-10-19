package edu.osu.cse.groenkeb.logic.utils

/**
 * Provides 'unapply' implementation for the empty Set case. Scala's Set implementation
 * doesn't provide 'unapply' by default, for good reason; Sets do not have a defined order.
 * However, it's still useful to be able to match against the empty vs non-empty set in pattern
 * matching.
 */
object Empty {
  /**
   * Extracts a Nil Seq from the set if empty, returns None if the Set is non-empty.
   */
  def unapplySeq[T](set: Set[T]): Option[Seq[T]] = if (set.isEmpty) Some(Nil) else None
}
