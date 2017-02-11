package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

import scala.collection.mutable.Queue

abstract class SentenceParser(tokenizer: Tokenizer)(implicit opMatcher: OperatorMatcher) extends Parser[String, Sentence, SentenceParserOpts] {
  def isValidAtomName(str: String): Boolean
  def parse(src: String, opts: SentenceParserOpts*): Sentence = {
    return null
  }

  private def parse(tokens: Queue[Token]): Sentence = {
    var next = tokens.dequeue()
    return null
  }

  private def nodeParserFrom(opts: SentenceParserOpts*): NodeParser = {
    var option = opts.find(x => x match { case Notation(t) => true })
    var notation = option.getOrElse[SentenceParserOpts](Notation("prefix")).asInstanceOf[Notation]
    notation match {
      case Notation("infix") => InfixNodeParser()
      case Notation("prefix") => PrefixNodeParser()
      case Notation("postfix") => PostfixNodeParser()
      case _ => throw new IllegalArgumentException("Notation type not recognized: " + notation.typestr)
    }
  }
  
  private def matchBinaryOp(str: String): BinaryOperator = {
    val op = opMatcher.opFor(str)
    op match {
      case x if x.isInstanceOf[BinaryOperator] => op.asInstanceOf[BinaryOperator]
      case Null() => null
    }
  }
  
  private def matchUnaryOp(str: String): UnaryOperator = {
    val op = opMatcher.opFor(str)
    op match {
      case x if x.isInstanceOf[UnaryOperator] => op.asInstanceOf[UnaryOperator]
      case Null() => null
    }
  }

  private sealed abstract class NodeParser {
    def parseNode(node: NodeToken): Sentence
    def parseTerm(node: TerminalToken): Sentence = {
      return null
    }
  }
  private case class InfixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = {
      val children = node.children
      children match {
        case NodeToken(c1) :: TerminalToken(x) :: NodeToken(c2) :: Nil =>
          BinarySentence(parseNode(NodeToken(c1)), parseNode(NodeToken(c2)), matchBinaryOp(x))
      }
    }
  }
  private case class PostfixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = {
      return null
    }
  }
  private case class PrefixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = {
      return null
    }
  }
}

sealed abstract class SentenceParserOpts
case class Notation(typestr: String) extends SentenceParserOpts

