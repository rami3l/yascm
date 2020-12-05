package io.github.rami3l.yascm

import scala.util.Try

/** A Scheme expression.
  */
sealed trait Exp

case class Bool(val value: Boolean) extends Exp
case class Sym(val value: String) extends Exp
case class Str(val value: String) extends Exp
case class Num(val value: Double) extends Exp

/** An unevaluated Scheme list.
  * Only used as an AST component (eg. when expressing function calls),
  * does not appear in evaluation results.
  */
case class ScmList(val value: List[Exp]) extends Exp

/** The special class signifying the end of a list.
  * Also used as an empty expression.
  */
case object ScmNil extends Exp

/** A `Cons` pair made up by two expressions.
  *
  * @param car The 1st expression.
  * @param cdr The 2nd expression.
  */
case class Cons[+T <: Exp](val car: T, val cdr: T) extends Exp

/** An anonymous function.
  *
  * `body := (List (List (vars) : defs))`
  */
case class Closure(val body: ScmList, val env: Env) extends Exp

case class Primitive(val value: List[Exp] => Try[Exp]) extends Exp
