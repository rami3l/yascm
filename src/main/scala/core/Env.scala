package io.github.rami3l.yascm

import collection.immutable.HashMap
import monocle.syntax.all._
import scala.compiletime.ops.boolean
import scala.annotation.tailrec
import cats.data.StateT
import cats.implicits._
import scala.util.{Try, Failure, Success}
import cats.data.State
import cats.data.OptionT

case class Env(
    val dict: HashMap[String, Exp] = HashMap(),
    val outer: STRef[Env, Option[Env]]
) {
  def insertVal(sym: String, defn: Exp): Env =
    this.focus(_.dict).modify(_.updated(sym, defn))
}

object Env {
  private def lookupWithSrc(
      sym: String
  ): StateT[Option, Env, (STRef[Env, Env], Exp)] = StateT { env =>
    env.dict
      .get(sym)
      // Either fortunately the definition of `sym` is in `this.dict`...
      .map { defn => Some(env, ((STRef(env), defn))) }
      // Or we should find it in `this.outer`.
      .getOrElse {
        val outer = env.outer.read.runA(env).value
        outer.flatMap(lookupWithSrc(sym).run(_))
      }
  }

  /** Find the definition of a symbol.
    */
  def lookup(sym: String): StateT[Option, Env, Exp] =
    lookupWithSrc(sym).map { case (_, defn) => defn }

  def setVal(
      sym: String,
      defn: Exp
  ): StateT[Option, Env, Unit] =
    lookupWithSrc(sym).map { case (envRef, _) =>
      envRef.modify(_.insertVal(sym, defn))
    }

  def handleLambda(func: Exp, args: List[Exp]): StateT[Try, Env, Exp] = for {
    func1 <- eval(func);
    args1 <- args.traverse(eval);
    res <- StateT.lift(func1.apply(args1))
  } yield res

  def evalList(exps: Seq[Exp]): StateT[Try, Env, Exp] =
    for { vs <- exps.traverse(eval) } yield vs.last

  def eval(exp: Exp): StateT[Try, Env, Exp] = {
    import cats.data.StateT.{inspectF, liftF, pure}
    import cats.data.IndexedStateT.{mapK}
    import OptionExt.successOrK

    def failureE(e: String) = Failure(Exception(e))

    exp match {
      // * Self-evaluating types.
      case n @ ScmInt(_)    => pure(n)
      case f @ ScmDouble(_) => pure(f)
      case s @ Str(_)       => pure(s)

      // * Booleans and other unchangeable constants.
      // No, we should not learn Python 2, where the booleans
      // are part of the prelude!
      case Sym("#t")  => pure(ScmBool(true))
      case Sym("#f")  => pure(ScmBool(false))
      case Sym("nil") => pure(ScmNil)

      // * Variable evaluation by name.
      case Sym(s) =>
        lookup(s).mapK {
          successOrK(Exception(s"eval: Symbol `$s` undefined"))
        }

      // * Function calls and keywords.
      case ScmList(Nil) =>
        liftF(failureE("eval: got empty function call"))
      // Inline anonymous function invocation.
      // eg. ((lambda (x) (+ x 2)) 3) ;; => 5
      case ScmList((func @ ScmList(_)) :: xs) =>
        handleLambda(func, xs)
      // Quote.
      case ScmList(Sym("quote") :: xs) =>
        xs match {
          case (l @ ScmList(_)) :: Nil => pure(l.toConsCell)
          case quotee :: Nil           => pure(quotee)
          case _ => liftF(failureE("quote: nothing to quote"))
        }
      // Anonymous function literal.
      // eg. (lambda (x y) *defns*)
      case ScmList(Sym("lambda") :: xs) =>
        for {
          // ! Here we want to clone a pointer, not to clone an Env.
          closEnv <- StateT.get
        } yield Closure(body = ScmList(xs), closEnv)
      // Definition.
      case ScmList(Sym("define") :: xs) =>
        xs match {
          // Simple definition.
          // eg. (define f (lambda (x y) *defns*))
          case Sym(sym) :: defn :: Nil => {
            val defn1 = env.eval(defn).get
            env.insertVal(sym, defn1)
            ScmNil
          }
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
            }.get
          case _ => throw Exception("define: nothing to define")
        }
      // Variable reset.
      case ScmList(Sym("set!") :: xs) =>
        xs match {
          // We can only reset a value that is already defined.
          case Sym(sym) :: defn :: Nil if env.lookup(sym).isDefined => {
            val defn1 = env.eval(defn).get
            env.setVal(sym, defn1)
            ScmNil
          }
          case _ => throw Exception("set!: nothing to set")
        }
      // Conditional expression.
      case ScmList(Sym("if") :: cond :: then1 :: else1 :: Nil) =>
        env.eval(cond).get match {
          case ScmBool(cond) => env.eval(if (cond) then1 else else1).get
          case _             => throw Exception("if: expected Bool")
        }
      case ScmList(Sym("if") :: _) => throw Exception("if: ill-formed")
      case ScmList(Sym("cond") :: tail) => {
        def evalTail(xs: List[Exp]): Try[Exp] = Try {
          xs match {
            case ScmList(Sym("else") :: then1 :: Nil) :: Nil =>
              env.eval(then1).get
            case ScmList(cond :: then1 :: Nil) :: xs1 =>
              env.eval(cond).get match {
                case ScmBool(true)  => env.eval(then1).get
                case ScmBool(false) => evalTail(xs1).get
                case _              => throw Exception("cond: expected Bool")
              }
            case _ => throw Exception("cond: ill-formed")
          }
        }
        evalTail(tail).get
      }
      case ScmList(Sym("begin") :: xs) => env.evalList(xs).get
      case ScmList(Sym("display") :: xs) => {
        xs.foreach(println)
        ScmNil
      }

      // Exit.
      case ScmList(Sym("exit") :: xs) => {
        def exit(code: Int): Exp = {
          System.exit(code)
          ScmNil
        }

        xs match {
          case Nil                 => exit(0)
          case ScmInt(code) :: Nil => exit(code)
          case _                   => throw Exception("exit: expected Int")
        }
      }
      // Call function by name.
      case ScmList((func @ Sym(_)) :: args) => handleLambda(func, args).get

      case _ => throw Exception("eval: unexpected expression")
    }
  }
}
