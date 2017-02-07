package edu.osu.cse.groenkeb.logic.parse

import scala.collection.LinearSeq

class NodeRecursiveTokenizer(delim: (Char, Char)) extends Tokenizer[NodeToken] {
  
  def this() = this(('(', ')'))
  
  private val delimStart = delim._1
  private val delimEnd = delim._2
  
  def tokenize(source: String): Seq[NodeToken] = {
    val tokens = Vector()
    return tokenize(source, tokens)
  }
  
  private def tokenize(str: String, tokens: Vector[NodeToken]): Vector[NodeToken] = {
    if (str == null || str.isEmpty()) return tokens
    str.charAt(0) match {
      case `delimStart` => return tokens :+ ParentNodeToken(tokenize(str.substring(1), Vector()))
      case `delimEnd` => return tokens
      case c if c.isWhitespace => return tokenize(str.substring(1), tokens)
      case _ => {
        val word = nextWord(str)
        return tokenize(str.substring(word.length()), tokens :+ TerminalNodeToken(word))
      }
    }
  }
  
  private def nextWord(str: String): String = str.substring(0, str.indexWhere(isTermChar) match {
    case x if x > 0 => x
    case x => str.length()
  })
  
  private def isTermChar(c: Char) = c match {
    case `delimEnd` => true
    case x if x.isWhitespace => true
    case _ => false
  }
}

abstract class NodeToken extends Token {
  def value: String
  override def toString() = value
}

case class TerminalNodeToken(str: String) extends NodeToken {
  def value = str
}

case class ParentNodeToken(val children: Vector[NodeToken]) extends NodeToken {
  def value = '[' + children.mkString(",") + ']'
}

