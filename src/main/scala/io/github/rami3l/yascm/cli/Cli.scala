package io.github.rami3l.yascm.cli

import io.github.rami3l.yascm.core._
import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode

object App extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.println("Welcome to yascm, a simple Scheme interpreter.")
      env <- ScmPrelude.boxEnv
      _ <- ScmInterpreter.repl(env)
    } yield ExitCode.Success
}
