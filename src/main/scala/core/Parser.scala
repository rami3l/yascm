package io.github.rami3l.yascm

import scala.util.parsing.combinator._
import scala.util.Try

object ScmParser extends JavaTokenParsers {
  def symbol: Parser[Exp] = {
    val letters = """a-zA-Z"""
    val digits = """0-9"""
    val symChars = """!#$%&|*+\-/:<=>?@^_~""""
    s"""[$letters$symChars][$letters$digits$symChars]*""".r ^^ Sym
  }

  def quoted: Parser[Exp] = "'" ~> expr ^^ { quotee =>
    ScmList(Sym("quote") :: quotee :: Nil)
  }

  def str: Parser[Exp] = stringLiteral ^^ { Str(_) }
  def int: Parser[Exp] = wholeNumber ^^ { i => ScmInt(i.toInt) }
  def decimal: Parser[Exp] = decimalNumber ^^ { f => ScmDouble(f.toDouble) }
  def number: Parser[Exp] = int | decimal
  def atom: Parser[Exp] = number | symbol

  def list: Parser[Exp] = nil | dottedList | regularList
  def nil: Parser[Exp] = "(" ~ ")" ^^ { _ => ScmNil }
  def regularList: Parser[Exp] = "(" ~> expr.+ <~ ")" ^^ ScmList

  def dottedList: Parser[Exp] = "(" ~> (expr ~ "." ~ expr) <~ ")" ^^ {
    case x ~ _ ~ y => Cons(car = x, cdr = y)
  }

  def expr: Parser[Exp] = str | quoted | list | atom

  def eliminateComments(s: String): String = {
    s.linesIterator
      .map { _.split(' ').takeWhile(w => !w.startsWith(";")).mkString(" ") }
      .mkString(sep = "\n")
  }

  def run(s: String): Try[List[Exp]] = Try {
    parse(expr.+, eliminateComments(s)).get
  }
}
