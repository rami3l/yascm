package io.github.rami3l.yascm

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.control.Breaks._
import scala.util.{Try, Success, Failure}

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
  def run(line: String, initEnv: Env): Try[String] =
    // First we need to parse the expressions.
    ScmParser
      .run(line)
      // Then we need to evaluated them.
      .flatMap { exps => initEnv.evalList(exps) }
      // Finally we focus on those ended with a successful evaluation.
      .map { exp => exp.toString }

  /** Evaluate a sequence of Scheme code `String`s, each consisting of one or
    * more Scheme expressions, and for each `String`, return the corresponding
    * output `String` or `Error`. Every expression in a `String` will be
    * evaluated if possible, and the final value is taken from the last one.
    *
    * @param lines
    *   The `String`s to be evaluated.
    * @param initEnv
    *   The environment to begin the evaluation.
    */
  def runStrings(lines: Seq[String], initEnv: Env): Seq[Try[String]] =
    lines.map { line => run(line, initEnv) }

  @tailrec
  final def repl(initEnv: Env): Unit = {
    val line = readLine(text = ">> ")
    if (!line.isEmpty) {
      print(run(line, initEnv).recover { case e => s"Error: $e" }.get)
    }
    repl(initEnv)
  }
}
