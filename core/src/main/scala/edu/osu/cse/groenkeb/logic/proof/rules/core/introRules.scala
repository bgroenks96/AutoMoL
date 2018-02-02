package edu.osu.cse.groenkeb.logic.proof.rules.core

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._

case object NegationIntroduction extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = "~I"
}

case object AndIntroduction extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = "&I"
}

case object OrIntroduction extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = "vI"
}

case object IfIntroduction extends Rule {
  def major(sentence: Sentence) = false
  
  def yields(sentence: Sentence) = false
  
  def params(major: Option[Sentence])(implicit context: ProofContext) = null
  
  def infer(args: RuleArgs)(implicit context: ProofContext) = null
  
  override def toString = ">I"
}
