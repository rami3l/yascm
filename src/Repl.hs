module Repl
    ( repl
    , runScheme
    , runStrings
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Function
import           Data.IORef
import qualified EvalApply                     as E
import qualified Parser                        as P
import           System.Console.Haskeline
import qualified Types                         as T

repl :: IORef T.Env -> IO ()
repl envBox = runInputT defaultSettings (loop envBox)
  where
    loop :: IORef T.Env -> InputT IO ()
    loop envBox' = do
        mLine <- getInputLine ">> "
        case mLine of
            Nothing   -> return ()
            Just line -> case P.run line of
                Left  e    -> outputStrLn . show $ e
                Right expr -> do
                    mval <- lift . runExceptT $ E.eval expr envBox'
                    case mval of
                        Right T.Empty -> return ()
                        Right val     -> outputStrLn $ "=> " ++ show val
                        Left  e       -> outputStrLn $ show e
        loop envBox'

-- Get a list of Scheme Expression String's, and return the corresponding output String's
runStrings :: [String] -> IORef T.Env -> IO [String]
runStrings xs envBox = foldl seedGen (return []) xs
  where
    seedGen mSeed x = do
        seed <- mSeed
        P.run x & either
            (\err -> return $ seed ++ [show err])
            (\expr -> do
                val <- runExceptT $ E.eval expr envBox
                return $ seed ++ [show val]
            )

-- Get a String of Scheme Expression's, then read and eval all of them
runScheme :: String -> IORef T.Env -> ExceptT T.ScmErr IO T.Exp
runScheme str envBox = do
    exprs <- except $ P.runList str
    E.evalList exprs envBox
