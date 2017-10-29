package edu.osu.cse.groenkeb.logic.encoding

import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof.rules._
import edu.osu.cse.groenkeb.logic.model.rules._
import edu.osu.cse.groenkeb.logic.model._
import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import scala.reflect.runtime.universe

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
  
  implicit val ruleDecoder: Decoder[Rule] = Decoder.instance { c =>
    c.downField("type").as[String] match {
      case Right("not-V") => Right(NegationVerification())
      case Right("not-F") => Right(NegationFalsification())
      case Right("and-V") => Right(AndVerification())
      case Right("and-F") => Right(AndFalsification())
      case Right("or-V") => Right(OrVerification())
      case Right("or-F") => Right(OrFalsification())
      case Right("cond-V") => Right(ConditionalVerification())
      case Right("cond-F") => Right(ConditionalFalsification())
      case Right("eq-V") => c.downField("domain").as[Domain] match {
        case Right(domain) => Right(ExistentialVerification(domain))
        case Left(failure) => Left(failure)
      }
      case Right("eq-F") => c.downField("domain").as[Domain] match {
        case Right(domain) => Right(ExistentialFalsification(domain))
        case Left(failure) => Left(failure)
      }
      case Right("uq-V") => c.downField("domain").as[Domain] match {
        case Right(domain) => Right(UniversalVerification(domain))
        case Left(failure) => Left(failure)
      }
      case Right("uq-F") => c.downField("domain").as[Domain] match {
        case Right(domain) => Right(UniversalFalsification(domain))
        case Left(failure) => Left(failure)
      }
      case Right(s) => Left(DecodingFailure("Unrecognized rule type: " + s, List(CursorOp.MoveUp)))
      case Left(failure) => Left(failure)
    }
  }
  
  implicit val ruleEncoder: Encoder[Rule] = Encoder.instance { (r: Rule) => r match {
    case NegationVerification() => Json.fromString("not-V")
    case NegationFalsification() => Json.fromString("not-F")
    case AndVerification() => Json.fromString("and-V")
    case AndFalsification() => Json.fromString("and-F")
    case OrVerification() => Json.fromString("or-V")
    case OrFalsification() => Json.fromString("or-F")
    case ConditionalVerification() => Json.fromString("cond-V")
    case ConditionalFalsification() => Json.fromString("cond-F")
    case ExistentialVerification(domain) => Json.fromFields(Seq(("type", Json.fromString("eq-V")),
                                                                ("domain", domain.asJson)))
    case ExistentialFalsification(domain) => Json.fromFields(Seq(("type", Json.fromString("eq-F")),
                                                                ("domain", domain.asJson)))
    case UniversalVerification(domain) => Json.fromFields(Seq(("type", Json.fromString("uq-V")),
                                                                ("domain", domain.asJson)))
    case UniversalFalsification(domain) => Json.fromFields(Seq(("type", Json.fromString("uq-F")),
                                                                ("domain", domain.asJson)))
  }}
}