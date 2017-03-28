package edu.osu.cse.groenkeb.logic.parse

import scala.collection.LinearSeq

trait Tokenizer {
  def tokenize(source: String): Seq[Token]
}

trait Token {
  def value: String
}

sealed case class TokenizerException(msg: String) extends Exception(msg)

sealed abstract class TokenBase extends Token {
  def value: String
  override def toString() = value
}

case class TerminalToken(str: String) extends TokenBase {
  def value = str
}

case class NodeToken(val children: Seq[Token]) extends TokenBase {
  def value = "(%s)".format(children.mkString(" "))
}

