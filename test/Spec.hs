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

    it "handles quotes" $ checkIO [("(quote (1 2 a))", "Right [1.0,2.0,a]")]

    it "handles quotes with shorthand"
        $ checkIO [("'(1 2 a)", "Right [1.0,2.0,a]")]

    it "handles var definition" $ checkIO
        [ ("(define x 3)"              , "Right ")
        , ("x"                         , "Right 3.0")
        , ("(define one (lambda () 1))", "Right ")
        , ("(one)"                     , "Right 1.0")
        , ("(+ (one) (+ 2 x))"         , "Right 6.0")
        ]
