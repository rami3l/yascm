module Main where
import qualified ScmPrelude                    as Scm
import           Repl
import           Data.IORef

main :: IO ()
main = do
    prelude <- newIORef Scm.prelude
    repl prelude
