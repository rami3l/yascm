module Repl
    ( repl
    , runScheme
    )
where

import qualified Types                         as T
import qualified Parser                        as P
import qualified EvalApply                     as E
import qualified ScmPrelude                    as Scm
import           Control.Monad.State
import           System.Console.Haskeline
import           Data.IORef

repl :: IORef T.Env -> IO ()
repl env = runInputT defaultSettings (loop env)
  where
    loop :: IORef T.Env -> InputT IO ()
    loop envBox = do
        mline <- getInputLine ">> "
        case mline of
            Nothing   -> loop envBox
            Just line -> case P.run line of
                Right expr -> do
                    mval <- liftIO $ E.eval expr envBox
                    case mval of
                        Right val ->
                            outputStrLn ("=> " ++ show val) >> loop envBox
                        Left e -> outputStrLn (show e) >> loop env
                Left e -> outputStrLn (show e) >> loop env


-- Get a list of Scheme Expression String's, and return the corresponding output String's
runScheme :: [String] -> IORef T.Env -> IO [String]
runScheme xs envBox = foldl seedGen (return []) xs  where
    seedGen mseed x = do
        seed <- mseed
        case P.run x of
            Left  err  -> return $ seed ++ [show err]
            Right expr -> do
                val <- E.eval expr envBox
                return $ seed ++ [show val]
