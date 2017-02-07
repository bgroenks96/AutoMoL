package edu.osu.cse.groenkeb.logic.proof;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.TreeTraverser;

import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.Sentences;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Conclusion;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Premise;
import edu.osu.cse.groenkeb.logic.proof.interfaces.Proof;
import edu.osu.cse.groenkeb.logic.proof.rules.Rule;

public final class DefaultConclusion implements Conclusion
{
  private final Proof proof;
  private final Sentence sentence;
  private final Premise major;
  private final Premise minor;
  private final Rule rule;
  
  public DefaultConclusion(Sentence sentence, Premise major, Premise minor, Rule rule)
  {
    this.sentence = sentence;
    this.major = major;
    this.minor = minor;
    this.rule = rule;
    this.proof = new DefaultProof(this, ImmutableSet.of(major, minor));
  }

  @Override
  public Proof getProof()
  {
    return proof;
  }

  @Override
  public Sentence getSentence()
  {
    return sentence;
  }

  @Override
  public Premise getMajor()
  {
    return major;
  }

  @Override
  public Premise getMinor()
  {
    return minor;
  }

  @Override
  public boolean hasMinor()
  {
    return !minor.getSentence().matches(Sentences.nil());
  }

  @Override
  public Rule getRule()
  {
    return rule;
  }

  @Override
  public TreeTraverser<Premise> getPremiseTree()
  {
    // TODO Auto-generated method stub
    return null;
  }
}
