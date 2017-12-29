package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._

object ProofContext {
  def apply(goal: Sentence,
            rules: RuleSet,
            available: Traversable[Premise]) = new ProofContext(goal, rules, available)
  
  def apply(goal: Sentence, available: Traversable[Premise] = Nil)(implicit rules: RuleSet) = new ProofContext(goal, available)
}

case class ProofContext (goal: Sentence,
                         rules: RuleSet,
                         available: Set[Premise],
                         depth: Int) {
  def this(goal: Sentence, rules: RuleSet, available: Traversable[Premise]) = this(goal, rules, available.toSet, 1)
  
  def this(goal: Sentence, available: Traversable[Premise] = Nil)(implicit rules: RuleSet) = this(goal, rules, available)
  
  def withAssumptions(newAssumptions: Assumption*) = ProofContext(goal, rules, available ++ newAssumptions, depth)
  
  def withGoal(newGoal: Sentence) = ProofContext(newGoal, rules, available, depth + 1)
  
  def restrict(premises: Premise*) = ProofContext(goal, rules, available.filterNot { p => premises.exists { r => r.matches(p) } }, depth)
  
  def fromAvailable(sentence: Sentence) = available.find { p => p.matches(sentence) }
  
  def hasGoal(sentence: Sentence) = goal.matches(sentence)
}
