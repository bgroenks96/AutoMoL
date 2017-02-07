package edu.osu.cse.groenkeb.logic.proof.interfaces;

import com.google.common.collect.TreeTraverser;

import edu.osu.cse.groenkeb.logic.proof.rules.Rule;

public interface Conclusion extends Premise
{
  /**
   * @return the major premise of the conclusion
   */
  Premise getMajor();
  
  /**
   * @return the minor premise of the conclusion, or EmptyPremise if there is none
   */
  Premise getMinor();
  
  /**
   * @return true if this conclusion has a minor premise, false otherwise
   */
  boolean hasMinor();
  
  /**
   * @return the rule used to produce the conclusion from the premises
   */
  Rule getRule();
  
  /**
   * @return a tree traverser for the premises of this conclusion
   */
  TreeTraverser<Premise> getPremiseTree();
}
