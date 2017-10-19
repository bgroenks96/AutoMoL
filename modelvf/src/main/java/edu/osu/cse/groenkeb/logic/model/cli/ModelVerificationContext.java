package edu.osu.cse.groenkeb.logic.model.cli;

import edu.osu.cse.groenkeb.logic.model.FirstOrderModel;

public class ModelVerificationContext implements Context
{
  private final FirstOrderModel model;
  
  public ModelVerificationContext(final FirstOrderModel model)
  {
    this.model = model;
  }
  
  public FirstOrderModel getModel()
  {
    return this.model;
  }
  
  public void printStatus()
  {
    System.out.println(model.toString());
  }
}
