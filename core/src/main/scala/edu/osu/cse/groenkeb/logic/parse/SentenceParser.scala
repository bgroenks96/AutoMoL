package edu.osu.cse.groenkeb.logic.parse

import scala.Left
import scala.Right

import edu.osu.cse.groenkeb.logic.Absurdity
import edu.osu.cse.groenkeb.logic.Atom
import edu.osu.cse.groenkeb.logic.AtomicSentence
import edu.osu.cse.groenkeb.logic.BinaryConnective
import edu.osu.cse.groenkeb.logic.BinarySentence
import edu.osu.cse.groenkeb.logic.NullOp
import edu.osu.cse.groenkeb.logic.QuantifiedSentence
import edu.osu.cse.groenkeb.logic.Quantifier
import edu.osu.cse.groenkeb.logic.Sentence
import edu.osu.cse.groenkeb.logic.UnaryConnective
import edu.osu.cse.groenkeb.logic.UnarySentence

case class SentenceParser(tokenizer: Tokenizer)(implicit opMatcher: OperatorMatcher) extends Parser[String, Sentence, SentenceParserOpts] {
  def this()(implicit opMatcher: OperatorMatcher) = this(new NodeRecursiveTokenizer())
  
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
      case NullOp() => throw ParserException("undefined or malformed operator: " + str);
      case _ => throw ParserException("illegal use of non-binary operator: " + str);
    }
  }
  
  private def matchUnaryOp(str: String): Either[UnaryConnective, Quantifier] = {
    val op = opMatcher.opFor(str)
    op match {
      case x if x.isInstanceOf[UnaryConnective] => Left(op.asInstanceOf[UnaryConnective])
      case x if x.isInstanceOf[Quantifier] => Right(op.asInstanceOf[Quantifier])
      case NullOp() => throw ParserException("undefined or malformed operator: " + str);
      case _ => throw ParserException("illegal use of non-unary operator: " + str);
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
      case TerminalToken("!") => Absurdity
      case TerminalToken(x) => AtomicSentence(matchAtom(x))
      case NodeToken(children) => parseNode(NodeToken(children))
    }
  }
  
  private case class PrefixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = node.children.toList match {
      case TerminalToken(op) :: left :: right :: Nil => BinarySentence(operand(left), operand(right), matchBinaryOp(op))
      case TerminalToken(op) :: unary :: Nil => matchUnaryOp(op).fold(u => UnarySentence(operand(unary), u), q => QuantifiedSentence(operand(unary), q))
      case TerminalToken(x) :: Nil => operand(TerminalToken(x))
      case _ => throw ParserException("invalid format for token group: %s".format(node.value))
    }
  }
  
  private case class PostfixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = node.children.toList match {
      case left :: right :: TerminalToken(op) :: Nil => BinarySentence(operand(left), operand(right), matchBinaryOp(op))
      case unary :: TerminalToken(op) :: Nil => matchUnaryOp(op).fold(u => UnarySentence(operand(unary), u), q => QuantifiedSentence(operand(unary), q))
      case TerminalToken(x) :: Nil => operand(TerminalToken(x))
      case _ => throw ParserException("invalid format for token group: %s".format(node.value))
    }
  }
  
  private case class InfixNodeParser() extends NodeParser {
    def parseNode(node: NodeToken): Sentence = node.children.toList match {
      case left :: TerminalToken(op) :: right :: Nil => BinarySentence(operand(left), operand(right), matchBinaryOp(op))
      case TerminalToken(op) :: unary :: Nil => matchUnaryOp(op).fold(u => UnarySentence(operand(unary), u), q => QuantifiedSentence(operand(unary), q))
      case TerminalToken(x) :: Nil => operand(TerminalToken(x))
      case _ => throw ParserException("invalid format for token group: %s".format(node.value))
    }
  }
}

sealed abstract class SentenceParserOpts
case class Notation(typestr: String) extends SentenceParserOpts

object Notation {
  val Infix = Notation("infix")
  val Prefix = Notation("prefix")
  val Postfix = Notation("postfix")
}

