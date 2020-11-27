package com.github.rami3l.yascm
import scala.util.{Try,Success,Failure}

extension (env: Env):
  def handleLambda(func: Exp, args: List[Exp]): Try[Exp] = Try {
      val func1 =  env.eval(func).get
      val args1 = args.map{ env.eval(_).get }
      apply(func1, args1).get
  }

  def evalList(exps: List[Exp]) : Try[Exp] = Try {
    exps.init.foreach{ env.eval(_).get }
    env.eval(exps.last).get 
  }

  def eval(exp: Exp) : Try[Exp] = Try {
    exp match {
      // * Self-evaluating types.
      case n@Num(_) => n
      case s@Str(_) => s

      // * Variable evaluation by name.
      case Sym(s) => env.lookup(s).getOrElse(throw new Exception(s"eval: Symbol `$s` undefined"))

      // * Function calls and keywords.
      case ScmList(Nil) => throw new Exception("eval: got empty function call")
      // Inline anonymous function invocation.
      // eg. ((lambda (x) (+ x 2)) 3) ;; => 5
      case ScmList((func@ScmList(_)) :: xs) => env.handleLambda(func, xs).get
      // Quote.
      case ScmList(Sym("quote") :: xs) => xs match {
        case quotee :: Nil => quotee
        case _ =>  throw new Exception("quote: nothing to quote")
      }
      // Anonymous function literal.
      // eg. (lambda (x y) *defns*)
      case ScmList(Sym("lambda") :: xs) => {
        // ! Here we want to clone a pointer, not to clone an Env.
        val closEnv = new Env(outer = env)
        Closure(body = ScmList(xs), closEnv)
      }
      // Definition.
      case ScmList(Sym("define") :: xs) => xs match {
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
        case ScmList((func@Sym(_)) :: args) :: defns => env.eval{
          ScmList(
            Sym("define")
            :: func
            :: ScmList(Sym("lambda") :: ScmList(args) :: defns)
            :: Nil
          )
        }.get
        case _ => throw new Exception("define: nothing to define")
      }
      // Variable reset.
      case ScmList(Sym("set!") :: xs) => xs match {
        // We can only reset a value that is already defined.
        case Sym(sym) :: defn :: Nil if env.lookup(sym).isDefined => {
          val defn1 = env.eval(defn).get
          env.setVal(sym, defn1)
          ScmNil
        }
        case _ => throw new Exception("set!: nothing to set")
      }
      // Conditional expression.
      case ScmList(Sym("if") :: cond :: then1 :: else1 :: Nil) => {
        env.eval(cond).get match {
          case Bool(true) => env.eval(then1).get
          case Bool(false) => env.eval(else1).get
          case _ => throw new Exception("if: expected Bool")
        }
      }
      case ScmList(Sym("if") :: _) => throw new Exception("if: ill-formed")
      case ScmList(Sym("cond") :: tail) => {
        def evalTail(xs: List[Exp]): Try[Exp] = Try { xs match {
          case ScmList(Sym("else") :: then1 :: Nil) :: Nil => env.eval(then1).get
          case ScmList(cond:: then1 :: Nil):: xs1 => 
            env.eval(cond).get match {
              case Bool(true) => env.eval(then1).get
              case Bool(false) => evalTail(xs1).get
              case _ => throw new Exception("cond: expected Bool")
            }
          case _ => throw new Exception("cond: ill-formed")
        }}
        evalTail(tail).get
      }
      case ScmList(Sym("display") :: xs) => {
        xs.foreach{ println(_) }
        ScmNil
      }
      // Call function by name.
      case ScmList((func@Sym(_)) :: args) => handleLambda(func, args).get

      case _ => throw new Exception("eval: unexpected expression")
    }
  }


def apply(func: Exp, args: List[Exp]): Try[Exp] = Try {func match {
  // `func` can only be Primitive or Closure.
  case Primitive(prim) => prim(args).get
  case Closure(body, env) => {
    val ScmList(ScmList(vars) :: defns) = body
    var localEnv = new Env(outer = env)
    vars.zip(args).map{ (ident, arg) => localEnv.insertVal(ident.asInstanceOf[Sym].value, arg) }
    localEnv.evalList(defns).get
  }
  case _ => throw new Exception("apply: unexpected expression")
}}