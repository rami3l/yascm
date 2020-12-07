package io.github.rami3l.yascm

import scala.util.Try
// import Numeric.Implicits._
// import Ordering.Implicits._

/** A Scheme expression.
  */
sealed trait Exp

case class Bool(val value: Boolean) extends Exp {
  override def toString: String = s"$value"
}

case class Sym(val value: String) extends Exp {
  override def toString: String = s"$value"
}

case class Str(val value: String) extends Exp {
  override def toString: String = s""""$value""""
}

case class ScmInt(val value: Int) extends Exp {
  override def toString: String = s"$value"
  def toScmDouble: ScmDouble = ScmDouble(value)

  def +(that: ScmInt): ScmInt = ScmInt(value + that.value)
  def -(that: ScmInt): ScmInt = ScmInt(value - that.value)
  def *(that: ScmInt): ScmInt = ScmInt(value * that.value)
  def /(that: ScmInt): ScmDouble = this.toScmDouble / that

  def +(that: ScmDouble): ScmDouble = ScmDouble(value + that.value)
  def -(that: ScmDouble): ScmDouble = ScmDouble(value - that.value)
  def *(that: ScmDouble): ScmDouble = ScmDouble(value * that.value)
  def /(that: ScmDouble): ScmDouble = this.toScmDouble / that
}

case class ScmDouble(val value: Double) extends Exp {
  override def toString: String = s"$value"

  def +(that: ScmDouble): ScmDouble = ScmDouble(value + that.value)
  def -(that: ScmDouble): ScmDouble = ScmDouble(value - that.value)
  def *(that: ScmDouble): ScmDouble = ScmDouble(value * that.value)
  def /(that: ScmDouble): ScmDouble = ScmDouble(value / that.value)

  def +(that: ScmInt): ScmDouble = ScmDouble(value + that.value)
  def -(that: ScmInt): ScmDouble = ScmDouble(value - that.value)
  def *(that: ScmInt): ScmDouble = ScmDouble(value * that.value)
  def /(that: ScmInt): ScmDouble = ScmDouble(value / that.value)
}

/** An unevaluated Scheme list.
  * Only used as an AST component (eg. when expressing function calls),
  * does not appear in evaluation results.
  */
case class ScmList(val value: List[Exp]) extends Exp {
  override def toString: String =
    value.map(_.toString).mkString(start = "(", sep = " ", end = ")")
}

/** The special class signifying the end of a list.
  * Also used as an empty expression.
  */
case object ScmNil extends Exp {
  override def toString: String = "()"
}

/** A `Cons` pair made up by two expressions.
  *
  * @param car The 1st expression.
  * @param cdr The 2nd expression.
  */
case class Cons[+T <: Exp](val car: T, val cdr: T) extends Exp {
  override def toString: String = s"($car . $cdr)"
}

/** An anonymous function.
  *
  * `body := (List (List (vars) : defs))`
  */
case class Closure(val body: ScmList, val env: Env) extends Exp {
  override def toString: String = body.value match {
    case ScmList(vars) :: _ => {
      val vars1 = vars
        .map(_.toString)
        .mkString(start = "[", sep = ",", end = "]")
      s"<Closure: $vars1>"
    }
    case _ => throw Exception("Error while stringifying closure")
  }
}

case class Primitive(val value: List[Exp] => Try[Exp]) extends Exp {
  override def toString: String = "<Primitive>"
}
