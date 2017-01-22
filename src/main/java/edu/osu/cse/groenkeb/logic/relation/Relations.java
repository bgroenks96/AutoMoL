package edu.osu.cse.groenkeb.logic.relation;

public final class Relations
{
  private static final AndRelation AND = new AndRelation();
  private static final TruthRelation TRUE = new TruthRelation();
  private static final NotRelation NOT = new NotRelation();
  
  public static AndRelation and()
  {
    return AND;
  }
  
  public static NotRelation not()
  {
    return NOT;
  }
  
  public static TruthRelation truth()
  {
    return TRUE;
  }
  
  private Relations()
  {
  }
}
