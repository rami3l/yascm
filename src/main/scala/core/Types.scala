package io.github.rami3l.yascm

import scala.util.Try
import scala.annotation.tailrec
import Numeric.Implicits._
import Ordering.Implicits._

/** A Scheme expression.
  */
sealed trait Exp

case class ScmBool(val value: Boolean) extends Exp {
  override def toString: String = if (value) "#t" else "#f"
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
}

case class ScmDouble(val value: Double) extends Exp {
  override def toString: String = s"$value"
}

/** An unevaluated Scheme list. Only used@ an AST component (eg. when expressing
  * function calls), does not appear in evaluation results.
  */
case class ScmList(val value: List[Exp]) extends Exp {
  override def toString: String =
    value.map(_.toString).mkString(start = "(", sep = " ", end = ")")

  private def toConsCellImpl(l: List[Exp]): ConsCell = {
    if (l.isEmpty) {
      ScmNil
    } else {
      Cons(car = l.head, cdr = toConsCellImpl(l.tail))
    }
  }

  def toConsCell: ConsCell = toConsCellImpl(value)
}

sealed trait ConsCell extends Exp {
  def isList: Boolean
}

/** The special class signifying the end of a list. Also regarded as an empty
  * list. Also used as an empty expression.
  */
case object ScmNil extends ConsCell {
  override def toString: String = "()"
  def isList: Boolean = true
}

/** A `Cons` pair made up by two expressions.
  *
  * @param car
  *   The 1st expression.
  * @param cdr
  *   The 2nd expression.
  */
case class Cons(val car: Exp, val cdr: Exp) extends ConsCell {
  def tryToList: Try[List[Exp]] = Try {
    this match {
      case Cons(car, ScmNil) => List(car)
      case Cons(car, cdr)    => car :: cdr.asInstanceOf[Cons].tryToList.get
    }
  }

  override def toString: String = {
    if (isList) {
      tryToList.get.mkString(start = "(", sep = " ", end = ")")
    } else {
      s"($car . $cdr)"
    }
  }

  @tailrec
  final def isList: Boolean = (car, cdr) match {
    case (_, ScmNil)         => true
    case (_, t @ Cons(_, _)) => t.isList
    case (_, _)              => false
  }
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

case class Primitive(val value: Seq[Exp] => Try[Exp]) extends Exp {
  override def toString: String = "<Primitive>"
}

object ExpUtils {
  def makeList(args: Seq[Exp]): Exp = args match {
    case Nil             => ScmNil
    case Seq(x)          => Cons(x, ScmNil)
    case Seq(x, xs @ _*) => Cons(x, makeList(xs))
  }
}
