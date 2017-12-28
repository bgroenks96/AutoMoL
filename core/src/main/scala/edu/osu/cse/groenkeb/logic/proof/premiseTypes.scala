package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs
import edu.osu.cse.groenkeb.logic.proof.rules.NullRule
import edu.osu.cse.groenkeb.logic.proof.rules.IdentityRule
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs

sealed abstract class Premise(val sentence: Sentence) {
  def matches(p: Premise) = this.sentence.matches(p.sentence)
  def matches(s: Sentence) = s.matches(sentence)
}
sealed case class Proof(conclusion: Sentence, rule: Rule, args: RuleArgs, undischarged: Set[Assumption]) extends Premise(conclusion)
sealed case class Assumption(s: Sentence) extends Premise(s) {
  // default proof from identity for assumption
  def proof = Proof(s, IdentityRule, Default.args(s), Set(this))
}

object Premises {
  def assume(sentences: Sentence*) = sentences map { s => Assumption(s) }
}

private object Default {
  def args(s: Sentence) = UnaryArgs(Proof(s, NullRule, EmptyArgs, Set()))
}
