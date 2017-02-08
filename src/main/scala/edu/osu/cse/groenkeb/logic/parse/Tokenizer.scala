package edu.osu.cse.groenkeb.logic.parse

import scala.collection.LinearSeq

trait Tokenizer[T <: Token] {
  def tokenize(source: String): Seq[T]
}

trait Token {
  def value: String
}

protected case class TokenizerException(msg: String) extends Exception(msg)

sealed abstract class TokenBase extends Token {
  def value: String
  override def toString() = value
}

case class TerminalToken(str: String) extends TokenBase {
  def value = str
}

case class NodeToken(val children: Seq[Token]) extends TokenBase {
  def value = '[' + children.mkString(",") + ']'
}

