package edu.osu.cse.groenkeb.automol.webx

final class StatefulGenerator[T](initial: T, genFunc: T => T) {
  var state = initial
  def next = {
    val current = state
    state = genFunc(current)
    current
  }
}
