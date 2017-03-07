package edu.osu.cse.groenkeb.logic.test;

public interface CommandProcessor<T extends Context>
{
  public interface Command<T>
  {
    /**
     * Executes the command with the current and context and returns the result.
     * @param current
     * @return the new context after execution
     */
    T execute(T current);
  }
  
  Command<T> tryParseNextCommand() throws InvalidCommandException;
}
