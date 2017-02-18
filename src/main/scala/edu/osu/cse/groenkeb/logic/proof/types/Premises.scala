package edu.osu.cse.groenkeb.logic.proof.types

import edu.osu.cse.groenkeb.logic._;

sealed abstract class Premise(val sentence: Sentence)
case class Assumption(val sentence: Sentence) extends Premise(sentence)
case class ProudPremise(val sentence: Sentence) extends Premise(sentence)
case class Conclusion(val conclusion: Sentence, val major: Premise, val minor: Premise*) extends Premise(conclusion)