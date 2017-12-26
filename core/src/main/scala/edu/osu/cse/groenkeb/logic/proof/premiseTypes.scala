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
case class NullPremise() extends Premise(Sentences.nil())
case class Conclusion(val conclusion: Sentence,
                      val rule: Rule,
                      val args: RuleArgs) extends Premise(conclusion) {
  def major = args.prems(0)
  def minors = args.prems.drop(1)
  var premiseCount = args.prems.length
}
case class Assumption(s: Sentence) extends Premise(s) {
  // default proof from identity for assumption
  def proof = Proof(s, IdentityRule, Default.args(s, this), Set(this))
}
case class ProudPremise(s: Sentence) extends Premise(s) {
  // default proof from identity for "proud" premise
  def proof = Proof(s, IdentityRule, Default.args(s, this), Set())
}

object Premises {
  def proud(sentences: Sentence*) = sentences map { s => ProudPremise(s) }
  def assume(sentences: Sentence*) = sentences map { s => Assumption(s) }
}

private object Default {
  def args(s: Sentence, p: Premise) = UnaryArgs(Proof(Conclusion(s, NullRule, EmptyArgs), Set()))
}
