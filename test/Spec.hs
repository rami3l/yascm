module Main where

import qualified Types                         as T
import qualified Parser                        as P
import qualified EvalApply                     as E
import qualified ScmPrelude                    as Scm
import qualified Repl
import           Control.Monad.State
import           Test.Hspec
import           Data.IORef

runPrelude :: [String] -> IO [String]
runPrelude xs = do
    preludeBox <- newIORef Scm.prelude
    Repl.runScheme xs preludeBox

checkIO xs = runPrelude (map fst xs) `shouldReturn` (map snd xs)

main :: IO ()
main = hspec $ do
    basics
    sugar
    environment
    general
    -- big

basics = describe "scheme-basics" $ do
    it "does simple addition" $ checkIO [("(+ 1 2)", "Right 3.0")]

    it "does nested addition" $ checkIO [("(+ 1 (* 2 3))", "Right 7.0")]

    it "handles quote" $ checkIO [("(quote (1 2 a))", "Right [1.0,2.0,a]")]

    it "handles quote shorthand" $ checkIO [("'(1 2 a)", "Right [1.0,2.0,a]")]

    it "handles var definition and return-only func" $ checkIO
        [ ("(define x 3)"              , "Right ")
        , ("x"                         , "Right 3.0")
        , ("(define one (lambda () 1))", "Right ")
        , ("(one)"                     , "Right 1.0")
        , ("(+ (one) (+ 2 x))"         , "Right 6.0")
        ]

    it "handles func definition with args" $ checkIO
        [ ("(define x 3)", "Right ")
        , ("x"           , "Right 3.0")
        , ("(define inc (lambda (x) (+ x 1)))", "Right ")
        , ("(inc 100)"   , "Right 101.0")
        , ("(inc x)"     , "Right 4.0")
        ]

    it "handles cond" $ checkIO
        [ ("(if #t 123 wtf)", "Right 123.0")
        , ("(if #f wtf 123)", "Right 123.0")
        , ("(cond (#f wtf0) (#f wtf1) (#t 456) (else wtf3))", "Right 456.0")
        , ("(cond (#f wtf0) (#f wtf1) (#f wtf2) (else 789))", "Right 789.0")
        ]

    it "handles eq" $ checkIO
        [ ("(define one (lambda () 1))"    , "Right ")
        , ("(= 1 1)"                       , "Right True")
        , ("(= 1 (one))"                   , "Right True")
        , ("(if (= 1 (one)) 123 wtf)"      , "Right 123.0")
        , ("(if (= (one) (+ 4 5)) wtf 123)", "Right 123.0")
        ]

    it "handles cons list (cons, car and cdr)" $ checkIO
        [ ("(car (cons 123 456))", "Right 123.0")
        , ("(cdr (cons 123 456))", "Right 456.0")
        , ("(define p (cons (cons 1 2) (cons 3 4)))", "Right ")
        , ("(cdr (car p))"       , "Right 2")
        , ("(cdr p)"             , "Right [3.0,4.0]")
        , ("p"                   , "Right [[1.0,2.0],[3.0,4.0]]")
        , ("(define l (cons 1 (cons 2 (cons 3 null))))", "Right ")
        , ("(car (cdr l))"       , "Right 2.0")
        , ("(cdr (cdr (cdr l)))" , "Right []")
        ]

    it "handles begin" $ checkIO
        [("(begin (define one (lambda () 1)) (+ (one) 2))", "Right 3.0")]

    it "handles multiline" $ checkIO
        [ ( "(begin                 \n\
            \   (define one         \n\
            \       (lambda () 1))  \n\
            \   (+ (one) 2))"
          , "Right 3.0"
          )
        ]

    it "handles multiline with comments" $ checkIO
        [ ( "(begin                             \n\
            \   (define one ; hey there         \n\
            \       ; generating the number 1   \n\
            \       ; some more comments...     \n\
            \       (lambda () 1))              \n\
            \       ;; even more...             \n\
            \   (+ (one) 2))"
          , "Right 3.0"
          )
        ]

    it "does inline lambda calculation" $ checkIO
        [ ( "((lambda (x y z)       \n\
            \       (+ x            \n\
            \          (+ y z))) 1  \n\
            \                    2  \n\
            \                    3)"
          , "Right 6.0"
          )
        ]

sugar = describe "scheme-sugar" $ do
    it "handles syntax sugar for lambda body" $ checkIO
        [ ( "((lambda (x y z)           \n\
            \       (quote whatever)    \n\
            \       (+ x                \n\
            \          (+ y z))) 1      \n\
            \                    2      \n\
            \                    3)"
          , "Right 6.0"
          )
        ]

    it "handles syntax sugar for func definition" $ checkIO
        [ ( "(define (add3 x y z)   \n\
            \   (+ x                \n\
            \      (+ y z)))"
          , "Right "
          )
        , ( "(add3 101 \n\
            \      102 \n\
            \      103))"
          , "Right 306.0"
          )
        ]

    it "handles syntax sugar for func body" $ checkIO
        [ ( "(define (three)                \n\
            \   (quote whatever)            \n\
            \   (define one (lambda () 1))  \n\
            \   (+ (one) 2))"
          , "Right "
          )
        , ("(three)", "Right 3.0")
        ]

environment = describe "scheme-environment" $ do
    it "does simple var assignment" $ checkIO
        [ ("(define inc (lambda (x) (+ x 1)))", "Right ")
        , ("(define x 3)"    , "Right ")
        , ("(set! x (inc x))", "Right ")
        , ("x"               , "Right 4.0")
        , ("(set! x (inc x))", "Right ")
        , ("x"               , "Right 5.0")
        ]

    it "passes the bank account test" $ checkIO
        [ ( "(define account                        \n\
            \   (lambda (bal)                       \n\
            \       (lambda (amt)                   \n\
            \           (begin                      \n\
            \               (set! bal (+ bal amt))  \n\
            \               bal))))"
          , "Right "
          )
        , ("(define a1 (account 100))", "Right ")
        , ("(a1 0)"                   , "Right 100.0")
        , ("(a1 10)"                  , "Right 110.0")
        , ("(a1 10)"                  , "Right 120.0")
        ]

general = describe "scheme-general" $ do
    it "calculates sqrt(200)" $ checkIO
        [ ("(define (abs x) (if (>= x 0) x (- 0 x)))", "Right ")
        , ( "(define (newton guess function derivative epsilon)                     \n\
            \   (define guess2 (- guess (/ (function guess) (derivative guess))))   \n\
            \   (if (< (abs (- guess guess2)) epsilon) guess2                       \n\
            \       (newton guess2 function derivative epsilon)))"
          , "Right "
          )
        , ( "(define (square-root a) \n\
            \   (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 1e-8))"
          , "Right "
          )
        , ("(> (square-root 200) 14.14213)", "Right True")
        , ("(< (square-root 200) 14.14215)", "Right True")
        ]

    it "calculates fibonacci numbers" $ checkIO
        [ ( "(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))"
          , "Right "
          )
        , ("(fib 20)", "Right 10946.0")
        , ( "(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))"
          , "Right "
          )
        , ( "(define map (lambda (f l) (if (null? l) null (cons (f (car l)) (map f (cdr l))))))"
          , "Right "
          )
        , ("(range 0 10)", "Right [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]")
        , ( "(map fib (range 0 10))"
          , "Right [1.0,1.0,2.0,3.0,5.0,8.0,13.0,21.0,34.0,55.0]"
          )
        ]

big = describe "scheme-big" $ do
    it "passes man_or_boy(4) test" $ checkIO
        [ ( "(define A (lambda (k x1 x2 x3 x4 x5)                           \n\
            \   (define B (lambda () (set! k (- k 1)) (A k B x1 x2 x3 x4))) \n\
            \   (if (<= k 0) (+ (x4) (x5)) (B))))"
          , "Right "
          )
        , ( "(A 4 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))"
          , "Right 1.0"
          )
        ]

    it "passes man_or_boy(10) test" $ checkIO
        [ ( "(define A (lambda (k x1 x2 x3 x4 x5)                           \n\
            \   (define B (lambda () (set! k (- k 1)) (A k B x1 x2 x3 x4))) \n\
            \  (if (<= k 0) (+ (x4) (x5)) (B))))"
          , "Right "
          )
        , ( "(A 4 (lambda () 1) (lambda () -1) (lambda () -1) (lambda () 1) (lambda () 0))"
          , "Right -67.0"
          )
        ]


