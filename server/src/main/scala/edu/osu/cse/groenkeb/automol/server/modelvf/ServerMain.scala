package edu.osu.cse.groenkeb.automol.server.modelvf

import cats.effect._
import cats.effect.implicits._
import org.http4s.server.blaze.BlazeBuilder
import scala.concurrent.ExecutionContext.Implicits.global

object ServerMain {
  def main(args: Array[String]): Unit = {
    BlazeBuilder[IO].bindHttp(8192, "0.0.0.0")
      .mountService(new ProofQueryService("webx").service, "/")
      .serve
      .run
      .unsafeRunSync()
    println("server terminating...")
  }
}
