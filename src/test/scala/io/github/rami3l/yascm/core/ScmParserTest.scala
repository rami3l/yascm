package io.github.rami3l.yascm.test

import io.github.rami3l.yascm.core._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.Matchers._

class ScmParserTest {
  def checkParse(pairs: (String, String)*): Unit = {
    pairs.foreach { case (input, expected) =>
      // Print all the expressions à la Lisp, seperated with spaces.
      assertThat(ScmParser.run(input).get.mkString(sep = " "), is(expected))
    }
  }

  @Test def simpleParsing = {
    val res = "(define inc (lambda (x) (+ x 1))) (inc 2)"
    checkParse(
      res -> res,
      "(define inc (lambda (x) (+ x 1)))\n(inc 2)" -> res,
      """(define inc
        |  (lambda (x)
        |    (+ x 1)))
        |(inc 2)""".stripMargin -> res
    )
  }

  @Test def handleComment = {
    val res = "(define inc (lambda (x) (+ x 1))) (inc 2)"
    checkParse(
      """(define inc (lambda (x) (+ x 1))) ; this is a function
        |(inc 2)""".stripMargin -> res,
      """(define inc (lambda (x) (+ x 1))) ; this is a function
        |(inc 2) ;; this is a function call""".stripMargin -> res,
      """(define inc
        |  ;; this is a function
        |  ; something else to say
        |  (lambda (x)
        |    (+ x 1)))
        |(inc 2)""".stripMargin -> res
    )
  }

  @Test def handleSyntaxSugar = checkParse(
    "(quote (1 2 a))" -> "(quote (1 2 a))",
    "'(1 2 a)" -> "(quote (1 2 a))",
    "(1 . 2)" -> "(1 . 2)",
    "(1 . 2.2)" -> "(1 . 2.2)",
    "(1.1 . 2)" -> "(1.1 . 2)",
    "(1. 2)" -> "(1.0 2)",
    "(1. . 2)" -> "(1.0 . 2)",
    "(1 .2)" -> "(1 0.2)",
    "(1 . .2)" -> "(1 . 0.2)"
  )
}
