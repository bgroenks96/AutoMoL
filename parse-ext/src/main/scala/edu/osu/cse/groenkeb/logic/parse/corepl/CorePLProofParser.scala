package edu.osu.cse.groenkeb.logic.parse.corepl

import atto._, atto.ParseResult._, Atto._
import cats.implicits._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.parse.ParserException
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._

final case object CorePLProofParser extends edu.osu.cse.groenkeb.logic.parse.Parser[String, Proof, PrologProofParserOpts] {
  
    val builder = new CoreProofBuilder()
    
    lazy val proof: Parser[Proof] = {
      (string("d(") ~> list <~ char(','), sentence <~ char(','), inference <~ char(')')).mapN {
        (prems, conc, inf) => inf match {
          case Left((rule, args)) => builder.proof(conc, rule, args, prems)
          case Right(`conc`) => builder.trivialProof(conc, prems)
          case Right(s) => throw new ParserException(s"malformed proof term: trivial inference premise $s does not match conclusion $conc")
        }
      }
    }
    
    lazy val inference: Parser[Either[(Rule, RuleArgs), Sentence]] = {
      rule.map { ruleAndArgs => Left(ruleAndArgs) }
    } | sentence.map { s => Right(s) }
    
  
    lazy val list: Parser[List[Sentence]] = {
      (char('[') ~> innerList <~ char(']')) |
      string("[]").map { _ => Nil }
    }
    
    lazy val innerList: Parser[List[Sentence]] = (
      for {
        head <- sentence
        _ <- char(',')
        tail <- innerList
      } yield List(head) ++ tail
    ) | sentence.map { s => List(s) }
    
    private def upcast(sentence: Sentence) = sentence
    
    lazy val sentence: Parser[Sentence] = (
      (string("not(") ~> sentence <~ string(")")).map { s => Not(s) } |
      (string("and(") ~> sentence <~ char(','), sentence <~ char(')')).mapN { (left, right) => upcast(And(left, right)) } |
      (string("or(") ~> sentence <~ char(','), sentence <~ char(')')).mapN { (left, right) => upcast(Or(left, right)) } |
      (string("if(") ~> sentence <~ char(','), sentence <~ char(')')).mapN { (left, right) => upcast(If(left, right)) } |
      char('#').map { _ => upcast(Absurdity) } |
      stringOf(letter).map { name => AtomicSentence(Atom(NamedPredicate(name))) }
    )
    
    private def upcast(appliedRule: (Rule, RuleArgs)) = appliedRule
    
    lazy val rule: Parser[(Rule, RuleArgs)] = (
      (string("not_i(") ~> proof <~ char(')')).map { major => (NegationIntroduction, UnaryArgs(major)) } |
      (string("not_e(") ~> sentence <~ char(','), proof <~ char(')')).mapN {
        (major, minor) => upcast(NegationElimination, BinaryArgs(builder.majorProof(major, minor), minor))
      } |
      (string("and_i(") ~> proof <~ char(','), proof <~ char(')')).mapN {
        (left, right) => upcast(AndIntroduction, BinaryArgs(left, right))
      } |
      (string("and_e(") ~> sentence <~ char(','), proof <~ char(')')).mapN {
        (major, minor) => upcast(AndElimination, BinaryArgs(builder.majorProof(major, minor), minor))
      } |
      (string("or_i(") ~> proof <~ char(')')).map { major => upcast(OrIntroduction, UnaryArgs(major)) } |
      (string("or_e(") ~> sentence <~ char(','), proof <~ char(','), proof <~ char(')')).mapN {
        (major, leftMinor, rightMinor) =>
          upcast(OrElimination, TernaryArgs(builder.majorProof(major, leftMinor, rightMinor), leftMinor, rightMinor))
      } |
      (string("if_i(") ~> proof <~ char(')')).map { major => upcast(IfIntroduction, UnaryArgs(major)) } |
      (string("if_e(") ~> sentence <~ char(','), proof <~ char(','), proof <~ char(')')).mapN {
        (major, anteMinor, consMinor) =>
          upcast(IfElimination, TernaryArgs(builder.majorProof(major, anteMinor, consMinor), anteMinor, consMinor))
      }
    )
  
  def parse(in: String, opts: PrologProofParserOpts): Proof = parse(in, Seq(opts))
  
  def parse(in: String, opts: Seq[PrologProofParserOpts]): Proof = {
    builder.reset
    
    proof.parseOnly(in) match {
      case Done(_, res) => res
      case Fail(in, _, msg) => throw ParserException(s"error parsing input: $in | $msg")
    }
  }
}

sealed abstract class PrologProofParserOpts
