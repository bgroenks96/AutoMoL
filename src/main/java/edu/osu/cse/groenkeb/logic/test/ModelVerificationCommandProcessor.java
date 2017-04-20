package edu.osu.cse.groenkeb.logic.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Stream;

import edu.osu.cse.groenkeb.logic.Absurdity;
import edu.osu.cse.groenkeb.logic.Sentence;
import edu.osu.cse.groenkeb.logic.Term;
import edu.osu.cse.groenkeb.logic.model.Domain;
import edu.osu.cse.groenkeb.logic.model.FirstOrderModel;
import edu.osu.cse.groenkeb.logic.parse.SentenceParser;
import edu.osu.cse.groenkeb.logic.parse.SentenceParserOpts;
import edu.osu.cse.groenkeb.logic.proof.NaiveProofStrategy;
import edu.osu.cse.groenkeb.logic.proof.ProofResult;
import edu.osu.cse.groenkeb.logic.proof.ProofSolver;
import edu.osu.cse.groenkeb.logic.proof.ProofUtils;
import edu.osu.cse.groenkeb.logic.proof.Success;
import edu.osu.cse.groenkeb.logic.proof.types.ProofContext;
import edu.osu.cse.groenkeb.logic.utils.Convert;

public class ModelVerificationCommandProcessor implements CommandProcessor<ModelVerificationContext>
{
  private static final String QUERY = "?";
  private static final String SET = "$";
  
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
  
  public QueryCommand parseQuery(final String input)
  {
    final Sentence querySentence = this.parser.parse(input, opts);
    return new QueryCommand(querySentence);
  }
  
  public SetCommand parseSet(final String input)
  {
    Domain auxDomain = new Domain(Convert.emptyScalaSeq());
    final List<Sentence> sentences = new ArrayList<Sentence>();
    for (final String pt : input.split(";"))
    {
      final String str = pt.trim();
      if (str.startsWith("{") && str.endsWith("}"))
      {
        final List<String> termStrs = Arrays.asList(str.substring(1, str.length() - 1).split(","));
        final Stream<Term> terms = termStrs.stream().map((String s) -> new Term(s));
        auxDomain = auxDomain.merge(new Domain(Convert.toScalaSeq(terms)));
      }
      else
      {
        sentences.add(this.parser.parse(pt, this.opts));
      }
    }
    
    final FirstOrderModel model = FirstOrderModel.from(Convert.toScalaSeq(sentences));
    return new SetCommand(model.withDomain(auxDomain));
  }
  
  private class QueryCommand implements Command<ModelVerificationContext>
  {
    private final Sentence sentence;
    private final ProofSolver solver = new ProofSolver(new NaiveProofStrategy());
    
    QueryCommand(final Sentence sentence)
    {
      this.sentence = sentence;
    }
    
    @Override
    public ModelVerificationContext execute(ModelVerificationContext current)
    {
      final ProofContext verifyProofContext = new ProofContext(this.sentence, Convert.emptyScalaSeq(), current.getModel().rules());
      final ProofContext falsifyProofContext = new ProofContext(new Absurdity(), Convert.emptyScalaSeq(), current.getModel().rules());
      final ProofResult verifyResult = solver.proof(verifyProofContext);
      if (verifyResult instanceof Success)
      {
        ProofUtils.prettyPrint(verifyResult.proof());
        return current;
      }
      
      final ProofResult falsifyResult = solver.proof(falsifyProofContext);
      ProofUtils.prettyPrint(falsifyResult.proof());
      return current;
    }
  }
  
  private class SetCommand implements Command<ModelVerificationContext>
  {
    private final FirstOrderModel model;
    
    SetCommand(FirstOrderModel model)
    {
      this.model = model;
    }
    
    public ModelVerificationContext execute(ModelVerificationContext current)
    {
      return new ModelVerificationContext(model);
    }
  }
}
