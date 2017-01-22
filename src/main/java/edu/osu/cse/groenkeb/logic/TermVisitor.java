package edu.osu.cse.groenkeb.logic;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import edu.osu.cse.groenkeb.logic.relation.Relation;
import edu.osu.cse.groenkeb.logic.relation.Relations;
import edu.osu.cse.groenkeb.logic.sentences.BinaryOperatorSentence;
import edu.osu.cse.groenkeb.logic.sentences.TermSentence;
import edu.osu.cse.groenkeb.logic.sentences.UnaryOperatorSentence;

public class TermVisitor
{
  private final Multimap<TermPair, Relation> termsToRelations = ArrayListMultimap.create ();
  private final Multimap<Relation, TermPair> relationsToTerms = ArrayListMultimap.create ();
  
  public void accept(Term term)
  {
    term.visit (this);
  }
  
  public void prop(Proposition prop)
  {
    addMember(pairFromSingleTerm(prop), Relations.truth ());
  }
  
  public void termSentence(TermSentence termSentence)
  {
    addMember(pairFromSingleTerm(termSentence.getTerm ()), Relations.truth ());
    termSentence.getTerm ().visit (this);
  }
  
  public void unarySentence(UnaryOperatorSentence unaryOpSentence)
  {
    TermPair terms = pairFromSingleTerm(unaryOpSentence);
    addMember(terms, unaryOpSentence.getOperator ().asRelation ());
    unaryOpSentence.getTerm ().visit (this);
  }
  
  public void binarySentence(BinaryOperatorSentence binaryOpSentence)
  {
    TermPair terms = new TermPair(binaryOpSentence.getFirstTerm (), binaryOpSentence.getSecondTerm ());
    addMember(terms, binaryOpSentence.getOperator ().asRelation ());
    terms.first.visit (this);
    terms.second.visit (this);
  }
  
  private void addMember(TermPair terms, Relation relation)
  {
    termsToRelations.put (terms, relation);
    relationsToTerms.put (relation, terms);
  }
  
  private TermPair pairFromSingleTerm(Term term)
  {
    return new TermPair(term, term);
  }
  
  private class TermPair
  {
    private final Term first, second;
    
    TermPair(final Term first, final Term second)
    {
      this.first = first;
      this.second = second;
    }
  }
}
