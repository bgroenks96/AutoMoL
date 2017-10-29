package edu.osu.cse.groenkeb.automol.server.modelvf

import cats.effect._
import cats.effect.implicits._
import fs2.Scheduler
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.util.StreamApp
import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s.util.StreamApp.ExitCode


object ServerMain extends StreamApp[IO] {
  def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, ExitCode] =
    Scheduler[IO](corePoolSize = 2).flatMap { implicit scheduler =>
      BlazeBuilder[IO]
        .bindHttp(8080)
        .mountService(new ModelVerificationService().service, "/")
        .serve
    }
}