package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Sentence

trait Model {
  def domain: Domain
  def diagram: AtomicDiagram
  def evaluate(sentence: ModelSentence): Boolean
}