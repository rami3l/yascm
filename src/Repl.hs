module Repl
    ( repl
    )
where

import qualified Types                         as T
import qualified Parser                        as P
import qualified EvalApply                     as E
import qualified ScmPrelude                    as Scm
import           Control.Monad.State

repl env = do
    putStr "> "
    line <- getLine
    case P.run line of
        Right exp ->
            let (val, env') = runState (E.eval exp) env
            in  print val >> repl env'
        Left e -> print e >> repl env

