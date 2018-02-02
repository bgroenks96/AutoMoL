package edu.osu.cse.groenkeb.logic.parse.prolog

import cats.Eval
import parseback._
import parseback.compat.cats._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.parse.ParserException
import edu.osu.cse.groenkeb.logic.proof._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.proof.rules.core._

final case object PrologProofParser extends edu.osu.cse.groenkeb.logic.parse.Parser[String, Proof, PrologProofParserOpts] {
  def parse(in: String, opts: PrologProofParserOpts): Proof = parse(in, Seq(opts))
  
  def parse(in: String, opts: Seq[PrologProofParserOpts]): Proof = {
    // implicit val W = Whitespace("""\s+"""r)
    
    lazy val proof: Parser[Proof] = (
      "d(" ~> list ~ "," ~ sentence ~ "," ~ derivation <~ ")" ^^ {
        (_, prems, _, conc, _, deriv) => deriv match {
          case Left((rule, args)) => Proof(conc, rule, args, prems.map { s => Assumption(s) }.toSet)
          case Right(sentence) =>
            val trivialProof = Assumption(sentence).proof
            Proof(conc, trivialProof.rule, trivialProof.args, prems.map { s => Assumption(s) }.toSet)
        }
      }
    )
    
    lazy val derivation: Parser[Either[(Rule, RuleArgs), Sentence]] = (
      rule ^^ { (_, rule, args) => Left((rule, args)) }
      | sentence ^^ { (_, s) => Right(s) }
    )
    
    lazy val list: Parser[List[Sentence]] = (
      "[" ~> innerList <~ "]" ^^ { (_, sentences) => sentences }
      | "[]" ^^ { (_,_) => Nil }
    )
    
    lazy val innerList: Parser[List[Sentence]] = (
      sentence ~ "," ~ innerList ^^ { (_, head, _, innerList) => List(head) ++ innerList }
      | sentence ^^ { (_, s) => List(s) }
    )
    
    lazy val sentence: Parser[Sentence] = (
      "not(" ~> sentence <~ ")" ^^ { (_, s) => Not(s) }
      | "and(" ~> sentence ~ "," ~ sentence <~")" ^^ { (_, left,_, right) => And(left, right) }
      | "or(" ~> sentence ~ "," ~ sentence <~")" ^^ { (_, left,_, right) => Or(left, right) }
      | "if(" ~> sentence ~ "," ~ sentence <~")" ^^ { (_, left,_, right) => Implies(left, right) }
      | "#" ^^ { (_, s) => Absurdity }
      | """[a-z]""".r ^^ { (_, name) => AtomicSentence(Atom(NamedPredicate(name))) }
    )
    
    lazy val rule: Parser[(Rule, RuleArgs)] = (
      "not_i(" ~> proof <~ ")" ^^ { (_, major) => (NegationIntroduction, UnaryArgs(major)) }
      | "not_e(" ~> sentence ~ "," ~ proof <~ ")" ^^ { (_, major,_, minor) => (NegationElimination, BinaryArgs(Assumption(major).proof, minor)) }
      | "and_i(" ~> proof ~ "," ~ proof <~ ")" ^^ { (_, left,_, right) => (AndIntroduction, BinaryArgs(left, right)) }
      | "and_e(" ~> sentence ~ "," ~ proof <~ ")" ^^ { (_, major,_, minor) => (AndElimination, BinaryArgs(Assumption(major).proof, minor)) }
      | "or_i(" ~> proof <~ ")" ^^ { (_, major) => (OrIntroduction, UnaryArgs(major)) }
      | "or_e(" ~> sentence ~ "," ~ proof ~ "," ~ proof <~ ")" ^^ {
        (_, major,_, leftMinor,_, rightMinor) => (OrElimination, TernaryArgs(Assumption(major).proof, leftMinor, rightMinor))
      }
      | "if_i(" ~> proof <~ ")" ^^ { (_, major) => (IfIntroduction, UnaryArgs(major)) }
      | "if_e(" ~> sentence ~ "," ~ proof ~ "," ~ proof <~ ")" ^^ {
        (_, major,_, anteMinor,_, consMinor) => (IfElimination, TernaryArgs(Assumption(major).proof, anteMinor, consMinor))
      }
    )
    
    proof(LineStream[Eval](in)).value.fold(
        errors => throw ParserException("One or more errors parsing input: " + errors.mkString(",")),
        proof => proof.toList.head
    )
  }
}

sealed abstract class PrologProofParserOpts
