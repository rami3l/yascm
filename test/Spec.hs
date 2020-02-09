module Main where

import qualified Types                         as T
import qualified Parser                        as P
import qualified EvalApply                     as E
import qualified ScmPrelude                    as Scm
import           Control.Monad.State
import           Test.Hspec

-- Get a list of Scheme Expression String's, and return the corresponding output String's
runScheme :: [String] -> T.Env -> ([String], T.Env)
runScheme xs env = foldl seedGen ([], env) xs  where
    seedGen (ys, e) mx = case P.run mx of
        Left  err -> (ys ++ [show err], e)
        Right x   -> let (y, e') = runState (E.eval x) e in (ys ++ [show y], e')

runPrelude :: [String] -> [String]
runPrelude xs = fst $ runScheme xs Scm.prelude

checkIO xs = runPrelude (map fst xs) `shouldBe` (map snd xs)

main :: IO ()
main = do
    basics

basics = hspec $ describe "basics" $ do
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
        [ ( "(begin \n\
            \   (define one \n\
            \       (lambda () 1)) \n\
            \   (+ (one) 2))"
          , "Right 3.0"
          )
        ]

    it "handles multiline with comments" $ checkIO
        [ ( "(begin \n\
            \   (define one ; hey there \n\
            \       ; generating the number 1 \n\
            \       ; some more comments...\n\
            \       (lambda () 1)) \n\
            \       ;; even more... \n\
            \   (+ (one) 2))"
          , "Right 3.0"
          )
        ]
