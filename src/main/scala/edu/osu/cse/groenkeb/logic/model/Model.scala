package edu.osu.cse.groenkeb.logic.model

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.rules.RuleSet

trait Model {
  def domain: Domain
  def diagram: AtomicDiagram
  def verify(sentence: Sentence): Boolean
  def rules: RuleSet
}