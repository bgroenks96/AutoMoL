package edu.osu.cse.groenkeb.logic.parse

import edu.osu.cse.groenkeb.logic._

import scala.collection.mutable.Queue

class SentenceParser(tokenizer: Tokenizer)(implicit opMatcher: OperatorMatcher) extends Parser[String, Sentence, SentenceParserOpts] {
  def parse(src: String, opts: SentenceParserOpts = Notation("prefix")): Sentence = {
    parse(src, List(opts))
  }
  
  def parse(src: String, opts: Seq[SentenceParserOpts]): Sentence = {
    val nodeParser = opts.find(o => o.isInstanceOf[Notation]).orElse(Option.apply(Notation("prefix"))).get match {
      case Notation("prefix") => PrefixNodeParser()
      case Notation("postfix") => PostfixNodeParser()
      case Notation("infix") => InfixNodeParser()
      case n => throw new ParserException("Unsupported notation type: " + n)
    }
    
    parse(nodeParser, tokenizer.tokenize(src).toList)
  }

  private def parse(parser: NodeParser, tokens: List[Token]): Sentence = tokens match {
    case NodeToken(children) :: Nil => parser.parseNode(NodeToken(children))
    case tokens => parser.parseNode(NodeToken(tokens))
  }

  private def nodeParserFrom(opts: SentenceParserOpts*): NodeParser = {
    var option = opts.find(x => x match { case Notation(t) => true })
    var notation = option.getOrElse[SentenceParserOpts](Notation("prefix")).asInstanceOf[Notation]
    notation match {
      case Notation("prefix") => PrefixNodeParser()
      case _ => throw new IllegalArgumentException("Notation type not recognized: " + notation.typestr)
    }
  }
  
  private def matchBinaryOp(str: String): BinaryConnective = {
    val op = opMatcher.opFor(str)
    op match {
      case x if x.isInstanceOf[BinaryConnective] => op.asInstanceOf[BinaryConnective]
      case NullOp() => throw ParserException("Unrecognized operator: " + str);
      case _ => throw ParserException("Illegal use of non-binary operator: " + str);
    }
  }
  
  private def matchUnaryOp(str: String): UnaryConnective = {
    val op = opMatcher.opFor(str)
    op match {
      case x if x.isInstanceOf[UnaryConnective] => op.asInstanceOf[UnaryConnective]
      case NullOp() => throw ParserException("Unrecognized operator: " + str);
      case _ => throw ParserException("Illegal use of non-unary operator: " + str);
    }
  }
  
  private def matchAtom(str: String): Atom = {
    val op = opMatcher.opFor(str)
    op match {
      case NullOp() => Atom.parse(str)
      case _ => throw ParserException("Found unexpected operator token: " + str)
    }
  }

  private sealed abstract class NodeParser {
    def parseNode(node: NodeToken): Sentence
    protected def operand(token: Token) = token match {
      case TerminalToken(x) => AtomicSentence(matchAtom(x))
      case NodeToken(children) => parseNode(NodeToken(children))
    }
  }
  
  private case class PrefixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = node.children.toList match {
      case TerminalToken(op) :: left :: right :: Nil => BinarySentence(operand(left), operand(right), matchBinaryOp(op))
      case TerminalToken(op) :: unary :: Nil => UnarySentence(operand(unary), matchUnaryOp(op))
      case TerminalToken(x) :: Nil => operand(TerminalToken(x))
      case _ => throw ParserException("Found malformed token node: " + node.value)
    }
  }
  
  private case class PostfixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = node.children.toList match {
      case left :: right :: TerminalToken(op) :: Nil => BinarySentence(operand(left), operand(right), matchBinaryOp(op))
      case unary :: TerminalToken(op) :: Nil => UnarySentence(operand(unary), matchUnaryOp(op))
      case TerminalToken(x) :: Nil => operand(TerminalToken(x))
      case _ => throw ParserException("Found malformed token node: " + node.value)
    }
  }
  
  private case class InfixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = node.children.toList match {
      case left :: TerminalToken(op) :: right :: Nil => BinarySentence(operand(left), operand(right), matchBinaryOp(op))
      case TerminalToken(op) :: unary :: Nil => UnarySentence(operand(unary), matchUnaryOp(op))
      case TerminalToken(x) :: Nil => operand(TerminalToken(x))
      case _ => throw ParserException("Found malformed token node: " + node.value)
    }
  }
}

sealed abstract class SentenceParserOpts
case class Notation(typestr: String) extends SentenceParserOpts

