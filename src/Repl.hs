module Repl
    ( repl
    )
where

import qualified Types                         as T
import qualified Parser                        as P
import qualified EvalApply                     as E
import           Control.Monad.State
import           System.Console.Haskeline

repl :: T.Env -> IO ()
repl env = runInputT defaultSettings (loop env)
  where
    loop :: T.Env -> InputT IO ()
    loop env = do
        mline <- getInputLine ">> "
        case mline of
            Nothing   -> loop env
            Just line -> case P.run line of
                Right expr ->
                    let (val, env') = runState (E.eval expr) env
                    in  outputStrLn ("=> " ++ show val) >> loop env'
                Left e -> outputStrLn (show e) >> loop env
