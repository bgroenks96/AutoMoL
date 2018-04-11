package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.NullRule

final case class Action(rule: Rule, major: Option[Sentence] = None)

object Action {
  def none = Action(NullRule, None)
}
