package edu.osu.cse.groenkeb.automol.webx

/**
 * Generic generator type for sequences of values.
 */
final class StatefulGenerator[T](initial: T, genFunc: T => T) {
  var state = initial
  def next = {
    val current = state
    state = genFunc(current)
    current
  }
}
