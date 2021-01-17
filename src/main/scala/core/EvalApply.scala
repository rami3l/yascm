package io.github.rami3l.yascm

import scala.util.{Try,Success,Failure}

extension (env: Env) {
  def handleLambda(func: Exp, args: List[Exp]): Try[Exp] = Try {
    val func1 = env.eval(func).get
    val args1 = args.map { env.eval(_).get }
    func1.apply(args1).get
  }

  def evalList(exps: Seq[Exp]): Try[Exp] = Try {
    exps.init.foreach { env.eval(_).get }
    env.eval(exps.last).get
  }

  def eval(exp: Exp): Try[Exp] = Try {
    exp match {
      // * Self-evaluating types.
      case n@ ScmInt(_) => n
      case f@ ScmDouble(_) => f
      case s@ Str(_) => s

      // * Booleans and other unchangeable constants.
      // No, we should not learn Python 2, where the booleans
      // are part of the prelude!
      case Sym("#t") => ScmBool(true)
      case Sym("#f") => ScmBool(false)
      case Sym("nil") => ScmNil

      // * Variable evaluation by name.
      case Sym(s) =>
        env
          .lookup(s)
          .getOrElse(throw Exception(s"eval: Symbol `$s` undefined"))

      // * Function calls and keywords.
      case ScmList(Nil) => throw Exception("eval: got empty function call")
      // Inline anonymous function invocation.
      // eg. ((lambda (x) (+ x 2)) 3) ;; => 5
      case ScmList((func@ ScmList(_)) :: xs) => env.handleLambda(func, xs).get
      // Quote.
      case ScmList(Sym("quote") :: xs) =>
        xs match {
          case (l@ ScmList(_)) :: Nil => l.toConsCell
          case quotee :: Nil => quotee
          case _             => throw Exception("quote: nothing to quote")
        }
      // Anonymous function literal.
      // eg. (lambda (x y) *defns*)
      case ScmList(Sym("lambda") :: xs) => {
        // ! Here we want to clone a pointer, not to clone an Env.
        val closEnv = env
        Closure(body = ScmList(xs), closEnv)
      }
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
          case ScmList((func@ Sym(_)) :: args) :: defns =>
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
      case ScmList(Sym("if") :: cond :: then1 :: else1 :: Nil) => {
        env.eval(cond).get match {
          case ScmBool(true)  => env.eval(then1).get
          case ScmBool(false) => env.eval(else1).get
          case _           => throw Exception("if: expected Bool")
        }
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
                case _           => throw Exception("cond: expected Bool")
              }
            case _ => throw Exception("cond: ill-formed")
          }
        }
        evalTail(tail).get
      }
      case ScmList(Sym("begin") :: xs) => env.evalList(xs).get
      case ScmList(Sym("display") :: xs) => {
        xs.foreach { println(_) }
        ScmNil
      }
      // Call function by name.
      case ScmList((func@ Sym(_)) :: args) => handleLambda(func, args).get

      case _ => throw Exception("eval: unexpected expression")
    }
  }
}

extension (func: Exp) {
  def apply(args: List[Exp]): Try[Exp] = Try {
    func match {
      // `func` can only be Primitive or Closure.
      case Primitive(prim) => prim(args).get
      case Closure(body, env) => {
        val ScmList(varsList :: defns) = body
        val localEnv = Env(outer = env)

        varsList match {
          case ScmList(vars) =>
            vars.zip(args).map { (ident, arg) =>
              localEnv.insertVal(ident.asInstanceOf[Sym].value, arg)
            }
          case ScmNil => {}
          case _      => throw Exception("apply: unexpected expression")
        }

        localEnv.evalList(defns).get
      }
      case _ => throw Exception("apply: unexpected expression")
    }
  }
}
