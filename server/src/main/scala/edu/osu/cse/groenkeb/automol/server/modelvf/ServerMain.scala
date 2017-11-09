package edu.osu.cse.groenkeb.automol.server.modelvf

import cats.effect._
import cats.effect.implicits._
import org.http4s.server.blaze.BlazeBuilder
import scala.concurrent.ExecutionContext.Implicits.global

object ServerMain {
  def main(args: Array[String]): Unit = {
    BlazeBuilder[IO].bindHttp(8080)
      .mountService(new ModelVerificationService().service, "/")
      .serve
      .run
      .unsafeRunSync()
    println("server terminating...")
  }
}
