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
import edu.osu.cse.groenkeb.logic.model._
import edu.osu.cse.groenkeb.logic.model.implicits._
import edu.osu.cse.groenkeb.logic.proof.engine._
import edu.osu.cse.groenkeb.logic.web.modelvf._
import edu.osu.cse.groenkeb.logic._
import edu.osu.cse.groenkeb.logic.proof._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

final class ModelVerificationService extends Http4sDsl[IO] {
  val solver = new ProofSolver(new NaiveProofStrategy())
  
  val service = HttpService[IO] {
    case GET -> Root => Ok()
    case req@GET -> Root / "query" => req.decode[Json] {
      json => Ok(handleProofQuery(json))
    }
    .handleErrorWith {
      case e => BadRequest(e.getMessage)
    }
  }
  
  private def handleProofQuery(json: Json): Json = {
    json.as[VerificationProofRequest] match {
      case Right(VerificationProofRequest(query, model)) => firstResult(query)(model).asJson
      case Left(_) => Json.Null
    }
  }
  
  private def firstResult(query: Sentence)(implicit model: FirstOrderModel): Proof = {
    val verifyContext = ProofContext(query)
    val falsifyContext = ProofContext(Absurdity, Seq(ProudPremise(query)))
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