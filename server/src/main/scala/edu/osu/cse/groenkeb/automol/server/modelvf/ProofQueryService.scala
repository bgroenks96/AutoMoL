package edu.osu.cse.groenkeb.automol.server.modelvf

import cats._
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode
import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.dsl._
import edu.osu.cse.groenkeb.logic.encoding.json._
import edu.osu.cse.groenkeb.logic.parse._
import edu.osu.cse.groenkeb.logic.parse.prolog._
import edu.osu.cse.groenkeb.logic.model._
import edu.osu.cse.groenkeb.logic.model.implicits._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.web.modelvf._
import edu.osu.cse.groenkeb.logic.web.core._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

final class ProofQueryService(resPrefix: String) extends Http4sDsl[IO] {
  private implicit val opMatcher = new DefaultFirstOrderOpMatcher()
  
  private implicit val strategy = new EvaluationProofStrategy()
  
  private val solver = new ProofSolver

  val service = HttpService[IO] {
    case req@GET -> Root => StaticFile.fromString(resPrefix + "/proofdisplay.html", Some(req)).getOrElseF(NotFound())
    case req@POST -> Root / "modelvf" / "query" => req.decode[Json] {
      json => Ok(handleProofQuery(json))
    }
    .handleErrorWith {
      case e => {
        println(String.format("error serving request %s: %s", req, e))
        BadRequest(e.getMessage)
      }
    }
    case req@POST -> Root / "parse" => req.decode[Json] {
      json => Ok(handleParseQuery(json))
    }
    case req@GET -> path => StaticFile.fromString(resPrefix + "/" + path, Some(req)).getOrElseF(NotFound())
    case _ => MethodNotAllowed()
  }
  
  private def handleParseQuery(json: Json): Json = {
    println("received parse query: " + json.noSpaces)
    json.as[ParseProofRequest] match {
      case Right(ParseProofRequest(PrologParser, input)) => {
        val parser = PrologProofParser
        try
        {
          println(input)
          val result = parser.parse(input.trim(), Nil)
          println(result)
          return result.asJson
        } catch {
          case ex: Exception => {
            ex.printStackTrace()
            throw ex
          }
        }
      }
      case Left(_) => Json.Null
    }
  }

  private def handleProofQuery(json: Json): Json = {
    println("received proof query: " + json.noSpaces)
    json.as[VerificationProofRequest] match {
      case Right(VerificationProofRequest(queryStr, modelDesc)) => {
        val parser = SentenceParser(NodeRecursiveTokenizer())
        val query = parser.parse(queryStr)
        implicit val model = FirstOrderModel(modelDesc.map { s => parser.parse(s) }:_*)
        try {
          val result = firstResult(query)
          println(result)
          result.asJson
        } catch {
          case ex: Exception => {
            ex.printStackTrace()
            throw ex
          }
        }
      }
      case Left(_) => Json.Null
    }
  }

  private def firstResult(query: Sentence)(implicit model: FirstOrderModel): Proof = {
    val verifyContext = ProofContext(query)
    val falsifyContext = ProofContext(Absurdity, Seq(Assumption(query)))
    val f1 = Future[Stream[ProofResult]] {
      solver.prove(verifyContext)
    }
    val f2 = Future[Stream[ProofResult]] {
      solver.prove(falsifyContext)
    }

    // Await async result and return first available proof
    Await.result(Future.sequence(List(f1, f2)), Duration(10, TimeUnit.SECONDS)).flatMap {
      results => results.collect { case r:Success => r.proof }
    }.head
  }
}
