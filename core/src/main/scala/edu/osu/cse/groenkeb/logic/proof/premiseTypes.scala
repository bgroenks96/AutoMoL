package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.Sentences
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs
import edu.osu.cse.groenkeb.logic.proof.rules.NullRule
import edu.osu.cse.groenkeb.logic.proof.rules.IdentityRule
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.UnaryArgs

sealed abstract class Premise(val sentence: Sentence, val binding: Option[Binding]) {
  def matches(p: Premise) = this.sentence.matches(p.sentence)
  def matches(s: Sentence) = s.matches(sentence)
}
sealed case class Proof(conclusion: Sentence, rule: Rule, args: RuleArgs, undischarged: Set[Assumption], b: Option[Binding] = None)
  extends Premise(conclusion, b) {
  
  def uses(s: Sentence)(implicit context: ProofContext) = undischarged find { a => a.matches(s) } match {
    case Some(Assumption(a, Some(IntBinding(b)))) if b == context.depth => true
    case _ => false
  }
}
sealed case class Assumption(s: Sentence, b: Option[Binding] = None) extends Premise(s, b) {
  def this(s: Sentence) = this(s, None)
  
  // default proof from identity for assumption
  def proof = Proof(s, IdentityRule, Default.args(s), Set(this))
  
  override def toString = "%s(%s)".format(b match { case Some(binding) => "[%s]".format(binding); case None => "" }, s)
}

object Premises {
  def assume(sentences: Sentence*) = sentences map { s => Assumption(s) }
}

private object Default {
  def args(s: Sentence) = UnaryArgs(Proof(s, NullRule, EmptyArgs, Set()))
}
