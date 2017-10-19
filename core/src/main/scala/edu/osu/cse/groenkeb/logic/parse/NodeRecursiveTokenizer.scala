package edu.osu.cse.groenkeb.logic.parse

import scala.collection.LinearSeq

class NodeRecursiveTokenizer(delim: (Char, Char), prevalidate: Boolean) extends Tokenizer {
  
  def this() = this(('(', ')'), true)
  def this(prevalidate: Boolean) = this(('(', ')'), prevalidate)
  
  private val delimStart = delim._1
  private val delimEnd = delim._2
  
  def tokenize(source: String): Seq[TokenBase] = {
    if (prevalidate) validate(source);
    tokenize(source, 0, Vector()).tokens
  }
  
  def validate(source: String) {
    source match {
      case s if source.count { x => x == delimStart } == source.count { x => x == delimEnd } => Unit
      case s => throw TokenizerException("Delimiter mismatch!")
    }
  }
    
  private def tokenize(src: String, ind: Int, tokens: Vector[TokenBase]): TokenResult = {
    if (ind >= src.length()) return TokenResult(tokens, 0)
    src.charAt(ind) match {
      case `delimStart` => tokenizeNode(src, ind, tokens, tokenize(src, ind + 1, Vector()))
      case `delimEnd` => return TokenResult(tokens, 1)
      case c if c.isWhitespace => tokenizeBlank(src, ind, tokens)
      case _ => tokenizeTerm(src, ind, tokens, nextWord(src, ind))
    }
  }
  
  private def tokenizeNode(src: String, ind: Int, tokens: Vector[TokenBase], nodeResult: TokenResult) = {
    val result = tokenize(src, ind + nodeResult.len + 1, tokens :+ NodeToken(nodeResult.tokens))
    TokenResult(result.tokens , result.len + nodeResult.len + 1)
  }
  
  private def tokenizeTerm(src: String, ind: Int, tokens: Vector[TokenBase], word: String) = {
    var result = tokenize(src, ind + word.length(), tokens :+ TerminalToken(word))
    TokenResult(result.tokens, result.len + word.length)
  }
  
  private def tokenizeBlank(src: String, ind: Int, tokens: Vector[TokenBase]) = {
    var nextResult = tokenize(src, ind + 1, tokens)
    TokenResult(nextResult.tokens, nextResult.len + 1)
  }
  
  private def nextWord(str: String, startInd: Int): String = {
    str.substring(startInd, str.indexWhere(isTerminatingChar, startInd) match {
    case x if x > 0 => x
    case x => str.length()
  })}

  private def isTerminatingChar(c: Char) = c match {
    case `delimEnd` => true
    case x if x.isWhitespace => true
    case _ => false
  }
}

private case class TokenResult(val tokens: Vector[TokenBase], val len: Int)

