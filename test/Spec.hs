module Main where

import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.Text.Lazy (Text)
import qualified Parser as P
import Relude hiding (Text)
import qualified Repl
import qualified ScmPrelude as Scm
import Test.Hspec
import Text.RawString.QQ

checkParseList :: [(Text, Text)] -> Expectation
checkParseList xs = map (show . P.runList . fst) xs `shouldBe` map snd xs

runPrelude :: [Text] -> IO [Text]
runPrelude xs =
  snd <$> do
    preludeBox <- newIORef Scm.prelude
    runWriterT $ Repl.runStrings xs preludeBox

checkIO :: [(Text, Text)] -> Expectation
checkIO xs = runPrelude (map fst xs) `shouldReturn` map snd xs

main :: IO ()
main = hspec $ do
  parser
  basics
  sugar
  environment
  general
  big

parser :: SpecWith ()
parser = describe "scheme-parser" $ do
  it "does simple parsing" $
    checkParseList
      [ ("1.23", "Right [1.23]"),
        ("-1.23", "Right [-1.23]"),
        ("+ 1", "Right [+,1]"),
        ("+1", "Right [1]"),
        ("- 1", "Right [-,1]"),
        ("- 1.23", "Right [-,1.23]"),
        ("-1", "Right [-1]"),
        ("-1.23", "Right [-1.23]"),
        ("1+ 2", "Right [1+,2]"),
        ("1+ -2", "Right [1+,-2]"),
        ("1- +2", "Right [1-,+2]"),
        ("1- -2", "Right [1-,-2]"),
        ("(+ 1 x)", "Right [(+ 1 x)]"),
        ( "(define inc (lambda (x) (+ x 1))) (inc 2)",
          "Right [(define inc (lambda (x) (+ x 1))),(inc 2)]"
        ),
        ( [r|(define inc (lambda (x) (+ x 1)))

          (inc 2)|],
          "Right [(define inc (lambda (x) (+ x 1))),(inc 2)]"
        ),
        ( [r|
          (define inc
            (lambda (x)
              (+ x 1)))
          (inc 2)|],
          "Right [(define inc (lambda (x) (+ x 1))),(inc 2)]"
        )
      ]

  it "handles comments" $
    checkParseList
      [ ( [r|
          (define inc (lambda (x) (+ x 1))) ; this is a function
          (inc 1)|],
          "Right [(define inc (lambda (x) (+ x 1))),(inc 1)]"
        ),
        ( [r|
          (define inc (lambda (x) (+ x 1))) ; this is a function
          (inc 2) ;; this is a function call|],
          "Right [(define inc (lambda (x) (+ x 1))),(inc 2)]"
        ),
        ( [r|
          ;; this is a function
          (define inc
            (lambda (x) (+ x 1)))
          (inc 3)
          ;; this is a comment|],
          "Right [(define inc (lambda (x) (+ x 1))),(inc 3)]"
        ),
        ( [r|
          (define inc
          ;; this is a function
          ;; something else
            (lambda (x)
              (+ x 1)))
          (inc 4)|],
          "Right [(define inc (lambda (x) (+ x 1))),(inc 4)]"
        )
      ]

basics :: SpecWith ()
basics = describe "scheme-basics" $ do
  it "does simple addition" $ checkIO [("(+ 1 2)", "3")]

  it "does nested addition" $ checkIO [("(+ 1 (* 2 3))", "7")]

  it "handles quote" $ checkIO [("(quote (1 2 a))", "(1 2 a)")]

  it "handles quote shorthand" $ checkIO [([r|'(1 2 a)|], "(1 2 a)")]

  it "handles var definition and return-only func" $
    checkIO
      [ ("(define x 3)", "()"),
        ("x", "3"),
        ("(define one (lambda () 1))", "()"),
        ("(one)", "1"),
        ("(+ (one) (+ 2 x))", "6")
      ]

  it "handles func definition with args" $
    checkIO
      [ ("(define x 3)", "()"),
        ("x", "3"),
        ("(define inc (lambda (x) (+ x 1)))", "()"),
        ("(inc 100)", "101"),
        ("(inc x)", "4")
      ]

  it "handles cond" $
    checkIO
      [ ("(if #t 123 wtf)", "123"),
        ("(if #f wtf 123)", "123"),
        ("(cond (#f wtf0) (#f wtf1) (#t 456) (else wtf3))", "456"),
        ("(cond (#f wtf0) (#f wtf1) (#f wtf2) (else 789))", "789")
      ]

  it "handles eq" $
    checkIO
      [ ("(define one (lambda () 1))", "()"),
        ("(= 1 1)", "#t"),
        ("(= 1 (one))", "#t"),
        ("(if (= 1 (one)) 123 wtf)", "123"),
        ("(if (= (one) (+ 4 5)) wtf 123)", "123")
      ]

  it "handles cons list (cons, car and cdr)" $
    checkIO
      [ ("(car (cons 123 456))", "123"),
        ("(cdr (cons 123 456))", "456"),
        ("(define p (cons (cons 1 2) (cons 3 4)))", "()"),
        ("(cdr (car p))", "2"),
        ("(cdr p)", "(3 . 4)"),
        ("p", "((1 . 2) . (3 . 4))"),
        ("(define l (cons 1 (cons 2 (cons 3 nil))))", "()"),
        ("(car (cdr l))", "2"),
        ("(cdr (cdr (cdr l)))", "()")
      ]

  it "handles begin" $
    checkIO
      [("(begin (define one (lambda () 1)) (+ (one) 2))", "3")]

  it "handles multiline" $
    checkIO
      [ ( "(begin                 \n\
          \   (define one         \n\
          \       (lambda () 1))  \n\
          \   (+ (one) 2))",
          "3"
        )
      ]

  it "handles multiline with comments" $
    checkIO
      [ ( "(begin                             \n\
          \   (define one ; hey there         \n\
          \       ; generating the number 1   \n\
          \       ; some more comments...     \n\
          \       (lambda () 1))              \n\
          \       ;; even more...             \n\
          \   (+ (one) 2))",
          "3"
        )
      ]

  it "does inline lambda calculation" $
    checkIO
      [ ( "((lambda (x y z)       \n\
          \       (+ x            \n\
          \          (+ y z))) 1  \n\
          \                    2  \n\
          \                    3)",
          "6"
        )
      ]

sugar :: SpecWith ()
sugar = describe "scheme-sugar" $ do
  it "handles syntax sugar for lambda body" $
    checkIO
      [ ( "((lambda (x y z)           \n\
          \       (quote whatever)    \n\
          \       (+ x                \n\
          \          (+ y z))) 1      \n\
          \                    2      \n\
          \                    3)",
          "6"
        )
      ]

  it "handles syntax sugar for func definition" $
    checkIO
      [ ( "(define (add3 x y z)   \n\
          \   (+ x                \n\
          \      (+ y z)))",
          "()"
        ),
        ( "(add3 101 \n\
          \      102 \n\
          \      103))",
          "306"
        )
      ]

  it "handles syntax sugar for func body" $
    checkIO
      [ ( "(define (three)                \n\
          \   (quote whatever)            \n\
          \   (define one (lambda () 1))  \n\
          \   (+ (one) 2))",
          "()"
        ),
        ("(three)", "3")
      ]

environment :: SpecWith ()
environment = describe "scheme-environment" $ do
  it "does simple var assignment" $
    checkIO
      [ ("(define inc (lambda (x) (+ x 1)))", "()"),
        ("(define x 3)", "()"),
        ("(set! x (inc x))", "()"),
        ("x", "4"),
        ("(set! x (inc x))", "()"),
        ("x", "5")
      ]

  it "passes the bank account test" $
    checkIO
      [ ( "(define account                       \n\
          \  (lambda (bal)                        \n\
          \      (lambda (amt)                    \n\
          \          (begin                       \n\
          \              (set! bal (+ bal amt))   \n\
          \              bal))))",
          "()"
        ),
        ("(define a1 (account 100))", "()"),
        ("(a1 0)", "100"),
        ("(a1 10)", "110"),
        ("(a1 10)", "120")
      ]

general :: SpecWith ()
general = describe "scheme-general" $ do
  it "calculates sqrt(200)" $
    checkIO
      [ ("(define (abs x) (if (>= x 0) x (- 0 x)))", "()"),
        ( "(define (newton guess function derivative epsilon)                     \n\
          \   (define guess2 (- guess (/ (function guess) (derivative guess))))   \n\
          \   (if (< (abs (- guess guess2)) epsilon) guess2                       \n\
          \       (newton guess2 function derivative epsilon)))",
          "()"
        ),
        ( "(define (square-root a) \n\
          \   (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 1e-8))",
          "()"
        ),
        ("(> (square-root 200) 14.14213)", "#t"),
        ("(< (square-root 200) 14.14215)", "#t")
      ]

  it "calculates fibonacci numbers" $
    checkIO
      [ ( "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))",
          "()"
        ),
        ("(fib 20)", "10946"),
        ( "(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))",
          "()"
        ),
        ( "(define map (lambda (f l) (if (nil? l) nil (cons (f (car l)) (map f (cdr l))))))",
          "()"
        ),
        ("(range 0 10)", "(0 1 2 3 4 5 6 7 8 9)"),
        ( "(map fib (range 0 10))",
          "(1 1 2 3 5 8 13 21 34 55)"
        )
      ]

  it "passes man_or_boy(4) test" $
    checkIO
      [ ( "(define A (lambda (k x1 x2 x3 x4 x5)                           \n\
          \   (define B (lambda () (set! k (- k 1)) (A k B x1 x2 x3 x4))) \n\
          \   (if (<= k 0) (+ (x4) (x5)) (B))))",
          "()"
        ),
        ( "(A 4 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))",
          "1"
        )
      ]

big :: SpecWith ()
big = describe "scheme-big" $ do
  it "passes man_or_boy(10) test" $
    checkIO
      [ ( "(define A (lambda (k x1 x2 x3 x4 x5)                           \n\
          \   (define B (lambda () (set! k (- k 1)) (A k B x1 x2 x3 x4))) \n\
          \  (if (<= k 0) (+ (x4) (x5)) (B))))",
          "()"
        ),
        ( "(A 10 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))",
          "-67"
        )
      ]
