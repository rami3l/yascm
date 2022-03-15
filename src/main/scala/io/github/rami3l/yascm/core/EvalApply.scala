package io.github.rami3l.yascm.core

import cats.data.EitherT
import cats.data.OptionT
import cats.effect.IO
import cats.effect.Ref
import cats.implicits._

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

extension (env: IORef[Env]) {
  def handleLambda(func: Exp, args: List[Exp]): IO[Exp] =
    for {
      func1 <- env.eval(func)
      args1 <- args.traverse(env.eval)
      res <- func1.apply(args1)
    } yield res

  def evalList(exps: Seq[Exp]): IO[Exp] = for {
    _ <- exps.init.traverse(env.eval)
    res <- env.eval(exps.last)
  } yield res

  def eval(exp: Exp): IO[Exp] =
    exp match {
      // * Self-evaluating types.
      case n @ ScmInt(_)    => n.pure
      case f @ ScmDouble(_) => f.pure
      case s @ Str(_)       => s.pure

      // * Booleans and other unchangeable constants.
      // No, we should not learn Python 2, where the booleans
      // are part of the prelude!
      case Sym("#t")  => ScmBool(true).pure
      case Sym("#f")  => ScmBool(false).pure
      case Sym("nil") => ScmNil.pure

      // * Variable evaluation by name.
      case Sym(s) =>
        for {
          defn <- env.lookup(s).value
          res <- IO.fromOption(defn)(Exception(s"eval: Symbol `$s` undefined"))
        } yield res

      // * Function calls and keywords.
      case ScmList(Nil) =>
        IO.raiseError(Exception("eval: got empty function call"))
      // Inline anonymous function invocation.
      // eg. ((lambda (x) (+ x 2)) 3) ;; => 5
      case ScmList((func @ ScmList(_)) :: xs) => env.handleLambda(func, xs)
      // Quote.
      case ScmList(Sym("quote") :: xs) =>
        xs match {
          case (l @ ScmList(_)) :: Nil => l.toConsCell.pure
          case quotee :: Nil           => quotee.pure
          case _ => IO.raiseError(Exception("quote: nothing to quote"))
        }
      // Anonymous function literal.
      // eg. (lambda (x y) *defns*)
      case ScmList(Sym("lambda") :: xs) => {
        // ! Here we want to clone a pointer, not to clone an Env.
        val closEnv = env
        Closure(body = ScmList(xs), closEnv).pure
      }
      // Definition.
      case ScmList(Sym("define") :: xs) =>
        xs match {
          // Simple definition.
          // eg. (define f (lambda (x y) *defns*))
          case Sym(sym) :: defn :: Nil =>
            for {
              defn1 <- env.eval(defn)
              _ <- env.insertVal(sym, defn1)
            } yield ScmNil
          // Syntax sugar for function definition.
          // eg. (define (f x y) *defns*)
          // ->  (define f (lambda (x y) *defns*))
          case ScmList((func @ Sym(_)) :: args) :: defns =>
            env.eval {
              ScmList(
                Sym("define")
                  :: func
                  :: ScmList(Sym("lambda") :: ScmList(args) :: defns)
                  :: Nil
              )
            }
          case _ => IO.raiseError(Exception("define: nothing to define"))
        }
      // Variable reset.
      case ScmList(Sym("set!") :: xs) => {
        lazy val ex = Exception("set!: nothing to set")
        xs match {
          // We can only reset a value that is already defined.
          case Sym(sym) :: defn :: Nil =>
            for {
              mDefn <- env.lookup(sym).value
              // Throw an error when sym is not defined in env.
              _ <- IO.fromOption(mDefn)(ex)
              res <- for {
                evDefn <- env.eval(defn)
                _ <- env.setVal(sym, evDefn)
              } yield ScmNil
            } yield res
          case _ => IO.raiseError(ex)
        }
      }
      // Conditional expression.
      case ScmList(Sym("if") :: cond :: then1 :: else1 :: Nil) =>
        for {
          cond <- env.eval(cond)
          res <- cond match {
            case ScmBool(cond) => env.eval(if (cond) then1 else else1)
            case _             => IO.raiseError(Exception("if: expected Bool"))
          }
        } yield res
      case ScmList(Sym("if") :: _) => IO.raiseError(Exception("if: ill-formed"))
      case ScmList(Sym("cond") :: tail) => {
        def evalTail(xs: List[Exp]): IO[Exp] =
          xs match {
            case ScmList(Sym("else") :: then1 :: Nil) :: Nil =>
              env.eval(then1)
            case ScmList(cond :: then1 :: Nil) :: xs1 =>
              env.eval(cond).flatMap {
                _ match {
                  case ScmBool(true)  => env.eval(then1)
                  case ScmBool(false) => evalTail(xs1)
                  case _ => IO.raiseError(Exception("cond: expected Bool"))
                }
              }
            case _ => IO.raiseError(Exception("cond: ill-formed"))
          }

        evalTail(tail)
      }
      case ScmList(Sym("begin") :: xs) => env.evalList(xs)
      case ScmList(Sym("display") :: xs) =>
        for {
          exps <- xs.traverse(eval)
          _ <- exps.traverse(IO.println)
        } yield ScmNil

      // Call function by name.
      case ScmList((func @ Sym(_)) :: args) => handleLambda(func, args)
      case _ => IO.raiseError(Exception("eval: unexpected expression"))
    }
}

extension (func: Exp) {
  def apply(args: List[Exp]): IO[Exp] = {
    lazy val ex = IO.raiseError(Exception("eval: unexpected expression"))
    func match {
      // `func` can only be Primitive or Closure.
      case Primitive(prim) => prim(args)
      case Closure(body, env) => {
        val ScmList(varsList :: defns) = body
        for {
          localEnv <- Ref.of[IO, Env](Env(outer = Some(env)))
          _ <- varsList match {
            case ScmList(vars) =>
              vars.zip(args).traverse {
                case (Sym(ident), arg) => localEnv.insertVal(ident, arg)
                case _ =>
                  IO.raiseError(Exception("eval: unexpected expression"))
              }
            case ScmNil => ScmNil.pure[IO]
            case _ => IO.raiseError(Exception("eval: unexpected expression"))
          }
          res <- localEnv.evalList(defns)
        } yield res
      }
      case _ => ex
    }
  }
}
