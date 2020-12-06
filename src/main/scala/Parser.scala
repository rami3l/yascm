package io.github.rami3l.yascm

import scala.util.parsing.combinator._

class ScmParser extends JavaTokenParsers {
  def firstChar(s: String): Char = s.toCharArray.head

  def letter: Parser[Char] = """[a-zA-Z]""".r ^^ firstChar
  def digit: Parser[Char] = """[0-9]""".r ^^ firstChar
  def symChar: Parser[Char] = """[!#$%&|*+\-/:<=>?@^_~"]""".r ^^ firstChar

  def symbol: Parser[Exp] = {
    val x = letter | symChar
    val xs = (letter | digit | symChar).* ^^ { _.toString }
    (x ~ xs) ^^ { case x ~ xs => Sym(s"$x$xs") }
  }

  def str: Parser[Exp] = stringLiteral ^^ { Str(_) }

  def int: Parser[Exp] = wholeNumber ^^ { i => ScmInt(i.toInt) }
  def decimal: Parser[Exp] = decimalNumber ^^ { f => ScmDouble(f.toDouble) }
  def number: Parser[Exp] = int | decimal

  def atom: Parser[Exp] = number | symbol

  def regularList: Parser[Exp] = {
    ("""\(\s*""".r ~ repsep(expr, """\s+""".r) ~ """\)""".r) ^^ {
      // ! Problems!
      case _ ~ res ~ _ => ScmList(res)
    }
  }

  def expr: Parser[Exp] = ???
}
