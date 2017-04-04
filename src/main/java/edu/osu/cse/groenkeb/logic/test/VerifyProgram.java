package edu.osu.cse.groenkeb.logic.test;

import java.util.Scanner;

import edu.osu.cse.groenkeb.logic.model.FirstOrderModel;
import edu.osu.cse.groenkeb.logic.parse.DefaultFirstOrderOpMatcher;
import edu.osu.cse.groenkeb.logic.parse.NodeRecursiveTokenizer;
import edu.osu.cse.groenkeb.logic.parse.Notation;
import edu.osu.cse.groenkeb.logic.parse.SentenceParser;
import edu.osu.cse.groenkeb.logic.test.CommandProcessor.Command;

public class VerifyProgram
{
  public static void main(String[] args)
  {
    try(final Scanner input = new Scanner(System.in))
    {
      final SentenceParser parser = new SentenceParser(new NodeRecursiveTokenizer(), new DefaultFirstOrderOpMatcher());
      final ModelVerificationCommandProcessor processor = new ModelVerificationCommandProcessor(input, parser, Notation.Prefix());
      Command<ModelVerificationContext> next = null;
      ModelVerificationContext context = new ModelVerificationContext(FirstOrderModel.empty());
      boolean done = false;
      do
      {
        try
        {
          if (next != null)
          {
            context = next.execute(context);
            System.out.println();
          }

          context.printStatus();
          System.out.print("> ");
          next = processor.tryParseNextCommand();
          done = next == null;
        }
        catch (Exception e)
        {
          System.out.println(e.getMessage() + "\n");
          next = null;
        }
      }
      while (!done);
    }
  }
}
