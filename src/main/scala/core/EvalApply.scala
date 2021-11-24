package io.github.rami3l.yascm

import scala.util.{Try, Success, Failure}
import cats.data.StateT
import cats.implicits._

extension (func: Exp) {
  def apply(args: List[Exp]): Try[Exp] = Try {
    lazy val ex = Exception("apply: unexpected expression");
    func match {
      // `func` can only be Primitive or Closure.
      case Primitive(prim) => prim(args).get
      case Closure(body, env) => {
        val ScmList(varsList :: defns) = body
        val localEnv: Env = (varsList match {
          case ScmList(vars) =>
            vars.zip(args).foldM(Env(outer = Some(env))) {
              case (_, (Sym(ident), arg)) =>
                Success(localEnv.insertVal(ident, arg))
              case _ => Failure(ex)
            }
          case ScmNil => Success(localEnv)
          case _      => Failure(ex)
        }).get
        Env.evalList(defns).runA(localEnv).get
      }
      case _ => throw ex
    }
  }
}
