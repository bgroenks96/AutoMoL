package edu.osu.cse.groenkeb.logic.proof

import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext
import edu.osu.cse.groenkeb.logic.proof.types.Premise
import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.And

class DefaultPremiseSelector extends PremiseSelector {
  def select(conclusion: Sentence)(implicit context: ProofContext): Seq[Premise] = {
    sort(context.premises.union(context.assumptions))
  }
  
  def sort(premises: Seq[Premise]): Seq[Premise] = premises.sortWith(ordering)
  
  def ordering(p1: Premise, p2: Premise): Boolean = (p1.sentence, p2.sentence) match {
    case (nonAtomicSentence, AtomicSentence(a)) => false
    case (BinarySentence(_,_,_), BinarySentence(_,_,And())) => false
    case _ => true
  }
}