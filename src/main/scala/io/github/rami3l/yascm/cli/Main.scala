package io.github.rami3l.yascm.cli

import io.github.rami3l.yascm.core._

object Main {
  def main(args: Array[String]): Unit = {
    println("Welcome to yascm, a simple Scheme interpreter.")
    ScmInterpreter.repl(ScmPrelude.env)
  }
}
