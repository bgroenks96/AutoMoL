package edu.osu.cse.groenkeb.logic.proof

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.TreeTraverser;

import edu.osu.cse.groenkeb.logic.Sentences;
import edu.osu.cse.groenkeb.logic.proof.interfaces._;
import edu.osu.cse.groenkeb.logic.proof.rules.NullRule;

case class EmptyPremise() extends Premise
{
  def proof = EmptyProof(EmptyConclusion(this))
  def sentence = Sentences.nil
  def getProof = proof
  def getSentence = sentence
}

case class EmptyConclusion(p: EmptyPremise = null) extends Conclusion
{
  // necessary to prevent recursive init
  // revisit this later; might indicate a type design issue
  private val mp = p match
  {
    case null => EmptyPremise()
    case _ => p
  }
  
  def major = mp
  def minor = mp
  def rule = NullRule()
  def getMajor = major
  def getMinor = minor
  def getRule = rule
  def getSentence = Sentences.nil()
  def getProof = EmptyProof(this)
  def getPremiseTree = new EmptyPremiseTreeTraverser()
  def hasMinor = false
}

case class EmptyProof(conc: Conclusion) extends Proof
{
  def getInitialPremises = ImmutableSet.of()
  def getConclusion = conc
}

class EmptyPremiseTreeTraverser() extends TreeTraverser[Premise]
{
  def children(p: Premise) = ImmutableSet.of()
}