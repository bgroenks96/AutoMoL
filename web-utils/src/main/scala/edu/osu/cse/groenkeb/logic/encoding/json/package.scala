package edu.osu.cse.groenkeb.logic.encoding

import edu.osu.cse.groenkeb.logic._
import cats.syntax.either._
import io.circe._

package object json {
  implicit val binaryConnectiveDecoder: Decoder[BinaryConnective] = Decoder.instance { c =>
    c.downField("type").as[String] match {
      case Right("And") => Right(And())
      case Right("Or") => Right(Or())
      case Right("Implies") => Right(Implies())
      case Right(s) => Left(DecodingFailure("Unrecognized connective type: " + s, List(CursorOp.MoveUp)))
      case Left(failure) => Left(failure)
    }
  }
  
  implicit val binaryConnectiveEncoder: Encoder[BinaryConnective] = Encoder.instance { (s: BinaryConnective) => s match {
    case And() => Json.fromString("And")
    case Or() => Json.fromString("Or")
    case Implies() => Json.fromString("Implies")
  }}
  
  implicit val unaryConnectiveDecoder: Decoder[UnaryConnective] = Decoder.instance { c =>
    c.downField("type").as[String] match {
      case Right("Not") => Right(Not())
      case Right(s) => Left(DecodingFailure("Unrecognized connective type: " + s, List(CursorOp.MoveUp)))
      case Left(failure) => Left(failure)
    }
  }
  
  implicit val unaryConnectiveEncoder: Encoder[UnaryConnective] = Encoder.instance { (s: UnaryConnective) => s match {
    case Not() => Json.fromString("Not")
  }}
  
  implicit val quantifierDecoder: Decoder[Quantifier] = Decoder.instance { c =>
    c.downField("type").as[String] match {
      case Right("Universal") => c.field("term").as[String].map { term => UniversalQuantifier(Term(term)) }
      case Right("Existential") => c.field("term").as[String].map { term => ExistentialQuantifier(Term(term)) }
      case Right(s) => Left(DecodingFailure("Unrecognized quantifier type: " + s, List(CursorOp.MoveUp)))
      case Left(failure) => Left(failure)
    }
  }
  
  implicit val quantifierEncoder: Encoder[Quantifier] = Encoder.instance { (s: Quantifier) => s match {
    case UniversalQuantifier(term) => Json.fromFields(Seq(("type", Json.fromString("Universal")), ("term", Json.fromString(term.name))))
    case ExistentialQuantifier(term) => Json.fromFields(Seq(("type", Json.fromString("Existential")), ("term", Json.fromString(term.name))))
  }}
}