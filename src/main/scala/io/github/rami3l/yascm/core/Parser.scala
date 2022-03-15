package io.github.rami3l.yascm.core

import scala.util.Try
import scala.util.parsing.combinator._

object ScmParser extends JavaTokenParsers {
  def symbol: Parser[Exp] = {
    val letters = """a-zA-Z"""
    val digits = """0-9"""
    val symChars = """!#$%&|*+\-/:<=>?@^_~""""
    s"""[$letters$symChars][$letters$digits$symChars]*""".r ^^ Sym.apply
  }

  def quoted: Parser[Exp] =
    "'" ~> expr ^^ (quotee => ScmList(Sym("quote") :: quotee :: Nil))

  def str: Parser[Exp] = stringLiteral ^^ { str =>
    Str(str.stripPrefix("\"").stripSuffix("\""))
  }

  def decimal: Parser[Exp] =
    """[+-]?(\.\d+|\d+\.\d*)""".r ^^ (f => ScmDouble(f.toDouble))

  def int: Parser[Exp] = """[+-]?\d+""".r ^^ (i => ScmInt(i.toInt))
  def number: Parser[Exp] = decimal | int

  def list: Parser[Exp] = nil | dottedList | regularList
  def nil: Parser[Exp] = "(" ~ ")" ^^ (_ => ScmNil)
  def regularList: Parser[Exp] = "(" ~> expr.+ <~ ")" ^^ ScmList.apply

  // In a dotted list, it is required to add some whitespace characters after
  // the dot, in order to avoid ambiguities with decimals.
  def dottedList: Parser[Exp] = "(" ~> (expr ~ """\.\s+""".r ~ expr) <~ ")" ^^ {
    case x ~ _ ~ y => Cons(car = x, cdr = y)
  }

  def expr: Parser[Exp] = number | str | quoted | list | symbol

  def eliminateComments(s: String): String =
    s.linesIterator
      .map { _.split(' ').takeWhile(w => !w.startsWith(";")).mkString(" ") }
      .mkString(sep = "\n")

  def run(s: String): Try[List[Exp]] = Try {
    parse(expr.+, eliminateComments(s)).get
  }
}
