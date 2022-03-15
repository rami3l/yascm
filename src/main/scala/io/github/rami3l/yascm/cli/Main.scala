package io.github.rami3l.yascm.cli

import io.github.rami3l.yascm.core._
import cats.effect.IO

object Main {
  def main(args: Array[String]): IO[Unit] = {
    IO { println("Welcome to yascm, a simple Scheme interpreter.") }
    for {
      env <- ScmPrelude.boxEnv
      _ <- ScmInterpreter.repl(env)
    } yield ()
  }
}
