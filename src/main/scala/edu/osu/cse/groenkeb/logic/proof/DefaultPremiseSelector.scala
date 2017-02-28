package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.ObjectRelation
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.proof.types.NullPremise
import edu.osu.cse.groenkeb.logic.SentenceRelation
import edu.osu.cse.groenkeb.logic.AtomicSentence

class DefaultPremiseSelector extends PremiseSelector {
  def select(conclusion: ObjectRelation)(implicit context: ProofContext): Seq[Premise] = {
    sort(context.premises.union(context.assumptions))
  }
  
  def sort(premises: Seq[Premise]): Seq[Premise] = premises.sortWith(ordering)
  
  def ordering(p1: Premise, p2: Premise): Boolean = (p1.sentence.toRelation, p2.sentence.toRelation) match {
    case (SentenceRelation(s), r) => r match {
      case SentenceRelation(s1) if SentenceRelation(s1).atomic && !SentenceRelation(s).atomic => false
      case _ => true
    }
  }
}