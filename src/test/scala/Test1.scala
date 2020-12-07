package io.github.rami3l.yascm.test

import io.github.rami3l.yascm._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.Matchers._
import scala.util.Try

/*
class Test1 {
  @Test def t1(): Unit = {
    assertThat(app.Main.msg, is("I was compiled by dotty :)"))
  }
}
 */

class ScmParserTest {
  def parse(s: String): Try[List[Exp]] = Try {
    ScmParser.parse(ScmParser.expr.+, s).get
  }

  def checkParseList(i: Seq[(String, String)]): Unit = {
    i.foreach { case (input, expected) =>
      assertThat(parse(input).get.mkString(sep = " "), is(expected))
    }
  }

  @Test def simpleParsing = checkParseList {
    val res = "(define inc (lambda (x) (+ x 1))) (inc 2)"
    Seq(
      res -> res
    )
  }

}
