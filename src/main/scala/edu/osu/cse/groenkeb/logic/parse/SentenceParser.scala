package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

import scala.collection.mutable.Queue

abstract class SentenceParser(tokenizer: Tokenizer) extends Parser[String, Sentence, SentenceParserOpts] {
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
  
  private def matchOp[T <: Operator](str: String): T = str match {
    //
  }

  private sealed abstract class NodeParser {
    def parse(node: NodeToken): Sentence
  }
  private case class InfixNodeParser() extends NodeParser {
    def parse(node: NodeToken): Sentence = {
      val children = node.children
      children match {
        case NodeToken(c1) :: TerminalToken(x) :: NodeToken(c2) :: Nil =>
          BinarySentence(parse(NodeToken(c1)), parse(NodeToken(c2)), And())
      }
    }
  }
  private case class PostfixNodeParser() extends NodeParser {
    def parse(node: NodeToken): Sentence = {

    }
  }
  private case class PrefixNodeParser() extends NodeParser {
    def parse(node: NodeToken): Sentence = {

    }
  }
}

sealed abstract class SentenceParserOpts
case class Notation(typestr: String) extends SentenceParserOpts

