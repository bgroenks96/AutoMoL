package edu.osu.cse.groenkeb.logic.parse

trait Parser[TInput, TOutput, TOptions] {
  def parse(in: TInput, opts: TOptions): TOutput
  def parse(in: TInput, opts: Seq[TOptions]): TOutput
}

sealed case class ParserException(msg: String) extends Exception(msg)