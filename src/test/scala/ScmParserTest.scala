package io.github.rami3l.yascm.test

import io.github.rami3l.yascm._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.Matchers._

class ScmParserTest {
  def checkParseList(i: Seq[(String, String)]): Unit = {
    i.foreach { case (input, expected) =>
      assertThat(ScmParser.run(input).get.mkString(sep = " "), is(expected))
    }
  }

  @Test def simpleParsing = checkParseList {
    val res = "(define inc (lambda (x) (+ x 1))) (inc 2)"
    Seq(
      res -> res,
      "(define inc (lambda (x) (+ x 1)))\n(inc 2)" -> res,
      """(define inc
        |  (lambda (x)
        |    (+ x 1)))
        |(inc 2)""".stripMargin -> res
    )
  }

  @Test def handleComment = checkParseList {
    val res = "(define inc (lambda (x) (+ x 1))) (inc 2)"
    Seq(
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
}
