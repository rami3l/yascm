package io.github.rami3l.yascm.test

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits._
import io.github.rami3l.yascm.core._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class ScmParserTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  def checkParse(pairs: (String, String)*) =
    pairs
      .traverse { case (input, expected) =>
        for {
          // Print all the expressions à la Lisp, seperated with spaces.
          tt <- IO.fromTry(ScmParser.run(input))
        } yield (tt.mkString(sep = " "), expected)
      }
      .asserting(pairs => pairs.map(_._1) should equal(pairs.map(_._2)))

  "simple parsing" in {
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

  "handle comment" in {
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

  "handle syntax sugar" in checkParse(
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
