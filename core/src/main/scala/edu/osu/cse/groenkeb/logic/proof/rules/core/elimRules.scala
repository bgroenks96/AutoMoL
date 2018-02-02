package edu.osu.cse.groenkeb.logic.proof.rules.core

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._

case object NegationElimination extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = "~E"
}

case object AndElimination extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = "&E"
}

case object OrElimination extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = "vE"
}

case object IfElimination extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = ">E"
}
