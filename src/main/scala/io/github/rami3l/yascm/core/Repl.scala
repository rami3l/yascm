package io.github.rami3l.yascm.core

import cats.effect.IO

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.Breaks._

object ScmInterpreter {

  /** Evaluate a Scheme code `String` consisting of one or more Scheme
    * expressions, and return the corresponding output `String` or `Error`.
    * Every expression will be evaluated if possible, and the final value is
    * taken from the last one.
    *
    * @param line
    *   The `String` to be evaluated.
    * @param initEnv
    *   The environment to begin the evaluation.
    */
  def run(line: String, initEnv: IORef[Env]): IO[String] = for {
    // First we need to parse the expressions.
    tt <- IO.fromTry(ScmParser.run(line))
    // Then we need to evaluated them, focusing on those ended with a successful evaluation.
    res <- initEnv.evalList(tt)
  } yield res.toString

  final def repl(initEnv: IORef[Env]): IO[Unit] =
    for {
      line <- IO.print(">> ") >> IO.readLine
      _ <- IO.unlessA(line.isEmpty) {
        run(line, initEnv)
          .handleError("Error: " + _)
          .flatMap(IO.println)
      }
      _ <- repl(initEnv)
    } yield ()
}
