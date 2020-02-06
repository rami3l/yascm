module Main where
import qualified ScmPrelude                    as Scm
import           Repl

main :: IO ()
main = repl Scm.prelude
