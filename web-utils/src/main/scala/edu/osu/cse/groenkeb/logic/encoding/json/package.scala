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
    c.value.asString match {
      case Some("And") => Right(And())
      case Some("Or") => Right(Or())
      case Some("Cond") => Right(Implies())
      case Some(s) => Left(DecodingFailure("Unrecognized binary connective identifier: " + s, List(CursorOp.MoveLast)))
      case None => Left(DecodingFailure("Invalid JSON type for binary connective", List(CursorOp.MoveLast)))
    }
  }
  
  implicit val binaryConnectiveEncoder: Encoder[BinaryConnective] = Encoder.instance { (s: BinaryConnective) => s match {
    case And() => Json.fromString("And")
    case Or() => Json.fromString("Or")
    case Implies() => Json.fromString("Cond")
  }}
  
  implicit val unaryConnectiveDecoder: Decoder[UnaryConnective] = Decoder.instance { c =>
    c.value.asString match {
      case Some("Not") => Right(Not())
      case Some(s) => Left(DecodingFailure("Unrecognized unary connective identifier: " + s, List(CursorOp.MoveLast)))
      case None => Left(DecodingFailure("Invalid JSON type for unary connective", List(CursorOp.MoveLast)))
    }
  }
  
  implicit val unaryConnectiveEncoder: Encoder[UnaryConnective] = Encoder.instance { (s: UnaryConnective) => s match {
    case Not() => Json.fromString("Not")
  }}
  
  implicit val quantifierDecoder: Decoder[Quantifier] = Decoder.instance { c =>
    c.downField("type").as[String] match {
      case Right("Universal") => c.downField("term").as[String].map { term => UniversalQuantifier(Term(term)) }
      case Right("Existential") => c.downField("term").as[String].map { term => ExistentialQuantifier(Term(term)) }
      case Right(s) => Left(DecodingFailure("Unrecognized quantifier type: " + s, List(CursorOp.MoveLast)))
      case Left(failure) => Left(failure)
    }
  }
  
  implicit val quantifierEncoder: Encoder[Quantifier] = Encoder.instance { (s: Quantifier) => s match {
    case UniversalQuantifier(term) => Json.fromFields(Seq(("type", Json.fromString("Universal")), ("term", Json.fromString(term.name))))
    case ExistentialQuantifier(term) => Json.fromFields(Seq(("type", Json.fromString("Existential")), ("term", Json.fromString(term.name))))
  }}
  
  implicit val ruleDecoder: Decoder[Rule] = Decoder.instance { c =>
    c.downField("type").as[String] match {
      case Right("model") => c.downField("desc").as[FirstOrderModel] match {
        case Right(model) => Right(ModelRule(model))
        case Left(failure) => Left(failure)
      }
      case Right("id") => Right(IdentityRule)
      case Right("nil") => Right(NullRule)
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
      case Right(s) => Left(DecodingFailure("Unrecognized rule type: " + s, List(CursorOp.MoveLast)))
      case Left(failure) => Left(failure)
    }
  }
  
  implicit val ruleEncoder: Encoder[Rule] = Encoder.instance { (r: Rule) => r match {
    case ModelRule(model) => Json.obj(("type", Json.fromString("model")), ("desc", model.asJson))
    case IdentityRule => Json.obj(("type", Json.fromString("id")))
    case NullRule => Json.obj(("type", Json.fromString("nil")))
    case NegationVerification() => Json.obj(("type", Json.fromString("not-V")))
    case NegationFalsification() => Json.obj(("type", Json.fromString("not-F")))
    case AndVerification() => Json.obj(("type", Json.fromString("and-V")))
    case AndFalsification() => Json.obj(("type", Json.fromString("and-F")))
    case OrVerification() => Json.obj(("type", Json.fromString("or-V")))
    case OrFalsification() => Json.obj(("type", Json.fromString("or-F")))
    case ConditionalVerification() => Json.obj(("type", Json.fromString("cond-V")))
    case ConditionalFalsification() => Json.obj(("type", Json.fromString("cond-F")))
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