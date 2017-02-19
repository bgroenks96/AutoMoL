package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic._;

sealed abstract class Premise(val sentence: Sentence)
case class Assumption(s: Sentence) extends Premise(s)
case class ProudPremise(s: Sentence) extends Premise(s)
case class Conclusion(val conclusion: Sentence, val major: Premise, val minor: Premise*) extends Premise(conclusion)