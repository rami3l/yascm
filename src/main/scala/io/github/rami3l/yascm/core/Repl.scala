package io.github.rami3l.yascm.core

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.control.Breaks._
import scala.util.{Try, Success, Failure}
import cats.effect.IO

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
    // Then we need to evaluated them.
    res <- initEnv.evalList(tt)
  } // Finally we focus on those ended with a successful evaluation.
  yield res.toString

  @tailrec
  final def repl(initEnv: IORef[Env]): IO[Unit] = {
    val line = readLine(text = ">> ")
    if (!line.isEmpty) {
      println(run(line, initEnv).handleError("Error: " + _))
    }
    repl(initEnv)
  }
}
