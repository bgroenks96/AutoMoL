package edu.osu.cse.groenkeb.logic.test;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel;
import edu.osu.cse.groenkeb.logic.parse.SentenceParser;
import edu.osu.cse.groenkeb.logic.parse.SentenceParserOpts;
import edu.osu.cse.groenkeb.logic.utils.Convert;

public class ModelVerificationCommandProcessor implements CommandProcessor<ModelVerificationContext>
{
  private static final String QUERY = "?";
  private static final String SET = "!";
  
  private final Scanner input;
  private final SentenceParser parser;
  private final SentenceParserOpts opts;
  
  public ModelVerificationCommandProcessor(final Scanner input, final SentenceParser parser, final SentenceParserOpts opts)
  {
    this.input = input;
    this.parser = parser;
    this.opts = opts;
  }
  
  @Override
  public Command<ModelVerificationContext> tryParseNextCommand() throws InvalidCommandException
  {
    final String next = input.nextLine();
    if (next == null || next.isEmpty()) return null;
    
    final String cmdStr = next.substring(1, next.length()).trim();
    try
    {
      if (next.startsWith(QUERY))
      {
        return parseQuery(cmdStr);
      }
      else if (next.startsWith(SET))
      {
        return parseSet(cmdStr);
      }
    }
    catch (Exception e)
    {
      throw new InvalidCommandException("unable to parse input: " + e.getMessage());
    }
    
    throw new InvalidCommandException("unrecognized command string: " + next);
  }
  
  public QueryCommand parseQuery(final String str)
  {
    final Sentence querySentence = this.parser.parse(str, opts);
    return new QueryCommand(querySentence);
  }
  
  public SetCommand parseSet(final String str)
  {
    final List<Sentence> sentences = new ArrayList<Sentence>();
    for (final String s : str.split(";"))
    {
      sentences.add(this.parser.parse(s.trim(), this.opts));
    }
    
    return new SetCommand(sentences);
  }
  
  private class QueryCommand implements Command<ModelVerificationContext>
  {
    private final Sentence sentence;
    
    QueryCommand(final Sentence sentence)
    {
      this.sentence = sentence;
    }
    
    @Override
    public ModelVerificationContext execute(ModelVerificationContext current)
    {
      System.out.println(current.getModel().verify(this.sentence));
      return current;
    }
  }
  
  private class SetCommand implements Command<ModelVerificationContext>
  {
    private final List<Sentence> sentences;
    
    SetCommand(final List<Sentence> sentences)
    {
      this.sentences = sentences;
    }
    
    public ModelVerificationContext execute(ModelVerificationContext current)
    {
      return new ModelVerificationContext(FirstOrderModel.from(Convert.toScalaSeq(sentences)));
    }
  }
}
