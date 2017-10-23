package edu.osu.cse.groenkeb.automol.server.modelvf

import cats._
import cats.effect._
import cats.implicits._
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.server._
import org.http4s.dsl._

class ModelVerificationServer extends Http4sDsl[IO] {
  val service = HttpService[IO] {
    case GET -> Root => Ok()
  }
}
