package io.github.rami3l.yascm.test

import io.github.rami3l.yascm._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.Matchers._

class ScmInterpreterTest {
  // Here we simply assume that there will be only one expression to parse.
  def parse(input: String): Exp = ScmParser.run(input).get.last

  def checkIo(pairs: (String, String)*): Unit = {
    // I feel a bit sad for Scala: the reference to `prelude` is immutable,
    // but the underlying data is mutable.
    val prelude = Prelude.env
    pairs.foreach { case (expStr, expected) =>
      val res = prelude.eval(parse(expStr)).get
      assertThat(res.toString, is(expected))
    }
  }

  @Test def lists = checkIo(
    "nil" -> "()",
    "'()" -> "()",
    "'(())" -> "(())",
    "(quote (1 2 a))" -> "(1 2 a)",
    "'(1 2 a)" -> "(1 2 a)",
    "'((1 . 2) (3 . 4))" -> "((1 . 2) (3 . 4))",
    "'((1 . 2) . (3 . 4))" -> "((1 . 2) . (3 . 4))",
    "'(1 . (2 . (a . ())))" -> "(1 2 a)"
  )

  @Test def basicCalc = checkIo(
    "(+ 1 2)" -> "3",
    "(+ 1 (* 2 3))" -> "7",
    "(+ 3 (* 2 1 3) 1)" -> "10",
    "(+ 102.1 1)" -> "103.1",
    "(/ 1 16)" -> "0.0625"
  )

  @Test def basicFuncDef = checkIo(
    "(define x 3)" -> "()",
    "x" -> "3",
    "(define y 101.1)" -> "()",
    "y" -> "101.1",
    "((lambda (x) (+ x 1)) 101.1)" -> "102.1",
    "(define inc (lambda (x) (+ x 1)))" -> "()",
    "(inc 100)" -> "101",
    "(inc x)" -> "4",
    "(inc 102.1)" -> "103.1"
  )
}
