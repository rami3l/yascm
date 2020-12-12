package io.github.rami3l.yascm.test

import io.github.rami3l.yascm._
import org.junit.Test
import org.junit.Assert._
import org.hamcrest.Matchers._

class ScmInterpreterTest {
  // Here we simply assume that there will be only one expression to parse.
  def parse(input: String): Exp = ScmParser.run(input).get.last

  def checkIo(pairs: (String, String)*): Unit = {
    // I feel a bit sad for Scala: the reference to `prelude` is immutable,
    // but the underlying data is mutable.
    val prelude = Prelude.env
    pairs.foreach { case (expStr, expected) =>
      val res = prelude.eval(parse(expStr)).get
      assertThat(res.toString, is(expected))
    }
  }

  @Test def lists = checkIo(
    "nil" -> "()",
    "'()" -> "()",
    "'(())" -> "(())",
    "(quote (1 2 a))" -> "(1 2 a)",
    "'(1 2 a)" -> "(1 2 a)",
    "'((1 . 2) (3 . 4))" -> "((1 . 2) (3 . 4))",
    "'((1 . 2) . (3 . 4))" -> "((1 . 2) . (3 . 4))",
    "'(1 . (2 . (a . ())))" -> "(1 2 a)"
  )

  @Test def basicCalc = checkIo(
    "(+ 1 2)" -> "3",
    "(+ 1 (* 2 3))" -> "7",
    "(+ 3 (* 2 1 3) 1)" -> "10",
    "(+ 102.1 1)" -> "103.1",
    "(/ 1 16)" -> "0.0625",
    "((lambda (x) (+ x 1)) 101.1)" -> "102.1",
    """((lambda (x y z)
      |    (+ x
      |       (+ y z))) 1
      |                 2
      |                 3)""".stripMargin -> "6"
  )

  @Test def basicFuncDef = checkIo(
    "(define x 3)" -> "()",
    "x" -> "3",
    "(define y 101.1)" -> "()",
    "y" -> "101.1",
    "((lambda (x) (+ x 1)) y)" -> "102.1",
    "(define inc (lambda (x) (+ x 1)))" -> "()",
    "(inc 100)" -> "101",
    "(inc x)" -> "4",
    "(inc 102.1)" -> "103.1",
    "(inc y)" -> "102.1",
    "(define one (lambda () 1))" -> "()",
    "(one)" -> "1",
    "(+ (one) (+ 2.0 x))" -> "6.0"
  )

  @Test def cond = checkIo(
    "(if #t 123 wtf)" -> "123",
    "(if #f wtf 123)" -> "123",
    "(cond (#f wtf0) (#f wtf1) (#t 456) (else wtf3))" -> "456",
    "(cond (#f wtf0) (#f wtf1) (#f wtf2) (else 789))" -> "789"
  )

  @Test def eq = checkIo(
    "(define one (lambda () 1))" -> "()",
    "(= 1 1)" -> "#t",
    "(= 1 (one))" -> "#t",
    "(= 1.2 (one))" -> "#f",
    "(>= 1 (one))" -> "#t",
    "(> 1 (one))" -> "#f",
    "(>= 1.2 1)" -> "#t",
    "(> 1.2 1)" -> "#t",
    "(<= 1 1)" -> "#t",
    "(<= 1 2.1)" -> "#t",
    "(if (= 1 (one)) 123 wtf)" -> "123",
    "(if (= (one) (+ 4 5)) wtf 123)" -> "123"
  )

  @Test def consAndList = checkIo(
    "(car (cons 123 456))" -> "123",
    "(cdr (cons 123 456))" -> "456",
    "(define p (cons (cons 1 2) (cons 3 4)))" -> "()",
    "(cdr (car p))" -> "2",
    "(cdr p)" -> "(3 . 4)",
    "p" -> "((1 . 2) . (3 . 4))",
    "(define l (cons 1 (cons 2 (cons 3 nil))))" -> "()",
    "(car (cdr l))" -> "2",
    "(cdr (cdr (cdr l)))" -> "()"
  )

  @Test def begin = checkIo(
    "(begin (define one (lambda () 1)) (+ (one) 2))" -> "3"
  )

  @Test def sugarLambdaBody = checkIo(
    """((lambda (x y z)
      |    (quote whatever)
      |    (+ x            
      |       (+ y z))) 1  
      |                 2  
      |                 3)""".stripMargin -> "6"
  )

  @Test def sugarFuncDef = checkIo(
    "(define (one) 1)" -> "()",
    "(+ (one) 2)" -> "3",
    "(define (inc x) (+ 1 x))" -> "()",
    "(inc 101.1)" -> "102.1",
    "(define (add3 x y z) (+ x (+ y z)))" -> "()",
    "(add3 101 102 103)" -> "306",
    """(define (three x)
      |    (quote whatever)
      |    (define one (lambda () 1))
      |    (+ (one) 2))""".stripMargin -> "()",
    "(three 114)" -> "3"
  )

  @Test def basicEnv = checkIo(
    "(define inc (lambda (x) (+ x 1)))" -> "()",
    "(define x 3)" -> "()",
    "(set! x (inc x))" -> "()",
    "x" -> "4",
    "(set! x (inc x))" -> "()",
    "x" -> "5"
  )

  @Test def bankAccount = checkIo(
    """(define account
      |    (lambda (bal)
      |        (lambda (amt)
      |            (begin
      |                (set! bal (+ bal amt))
      |                bal))))""".stripMargin -> "()",
    "(define a1 (account 100))" -> "()",
    "(a1 0)" -> "100",
    "(a1 10)" -> "110",
    "(a1 10)" -> "120"
  )

  @Test def sqrt200 = checkIo(
    "(define (abs x) (if (>= x 0) x (- 0 x)))" -> "()",
    """(define (newton guess function derivative epsilon)
      |    (define guess2 (- guess (/ (function guess) (derivative guess))))
      |        (if (< (abs (- guess guess2)) epsilon) guess2
      |    (newton guess2 function derivative epsilon)))""".stripMargin -> "()",
    """(define (square-root a)
      |    (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 0.00000001))""".stripMargin -> "()",
    "(> (square-root 200) 14.14213)" -> "#t",
    "(< (square-root 200) 14.14215)" -> "#t"
  )

  @Test def fibonacci = checkIo(
    "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))" -> "()",
    "(fib 20)" -> "10946",
    "(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))" -> "()",
    "(define map (lambda (f l) (if (nil? l) nil (cons (f (car l)) (map f (cdr l))))))" -> "()",
    "(range 0 10)" -> "(0 1 2 3 4 5 6 7 8 9)",
    "(map fib (range 0 10))" -> "(1 1 2 3 5 8 13 21 34 55)"
  )

  @Test def manOrBoy4 = checkIo(
    """(define A (lambda (k x1 x2 x3 x4 x5)
      |    (define B (lambda () (set! k (- k 1)) (A k B x1 x2 x3 x4)))
      |        (if (<= k 0) (+ (x4) (x5)) (B))))""".stripMargin -> "()",
    "(A 4 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))" -> "1"
  )

  @Test def manOrBoy10 = checkIo(
    """(define A (lambda (k x1 x2 x3 x4 x5)
      |    (define B (lambda () (set! k (- k 1)) (A k B x1 x2 x3 x4)))
      |        (if (<= k 0) (+ (x4) (x5)) (B))))""".stripMargin -> "()",
    "(A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))" -> "-67"
  )
}
