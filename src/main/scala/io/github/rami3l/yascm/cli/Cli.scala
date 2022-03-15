package io.github.rami3l.yascm.cli

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import io.github.rami3l.yascm.core._

object App extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.println("Welcome to yascm, a simple Scheme interpreter.")
      env <- ScmPrelude.boxEnv
      _ <- ScmInterpreter.repl(env)
    } yield ExitCode.Success
}
