package io.github.rami3l.yascm

import scala.util.parsing.combinator._

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

  def list: Parser[Exp] = dottedList | regularList
  def regularList: Parser[Exp] = "(" ~> expr.* <~ ")" ^^ ScmList

  def dottedList: Parser[Exp] = "(" ~> (expr ~ "." ~ expr) <~ ")" ^^ {
    case x ~ _ ~ y => ScmList(x :: y :: Nil)
  }

  def expr: Parser[Exp] = str | quoted | list | atom
}
