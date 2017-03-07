package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.Sentences;
import edu.osu.cse.groenkeb.logic.ObjectRelation;
import edu.osu.cse.groenkeb.logic.proof.rules.Rule
import edu.osu.cse.groenkeb.logic.proof.rules.RuleArgs
import edu.osu.cse.groenkeb.logic.proof.rules.EmptyArgs
import edu.osu.cse.groenkeb.logic.proof.rules.NullRule

sealed abstract class Premise(val sentence: Sentence)
case class NullPremise() extends Premise(Sentences.nil())
case class Assumption(s: Sentence) extends Premise(s)
case class Conclusion(val conclusion: Sentence,
                      val rule: Rule,
                      val args: RuleArgs) extends Premise(conclusion)
case class ProudPremise(s: Sentence) extends Premise(s) {
  def proof = CompleteProof(Conclusion(s, NullRule(), EmptyArgs()), this :: Nil)
}
