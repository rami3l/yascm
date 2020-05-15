module Repl
    ( repl
    , runScheme
    , runStrings
    )
where

import qualified Types                         as T
import qualified Parser                        as P
import qualified EvalApply                     as E
import           Control.Monad.State
import           System.Console.Haskeline
import           Data.IORef

repl :: IORef T.Env -> IO ()
repl envBox = runInputT defaultSettings (loop envBox)
  where
    loop :: IORef T.Env -> InputT IO ()
    loop envBox = do
        mline <- getInputLine ">> "
        case mline of
            Nothing   -> return ()
            Just line -> case P.run line of
                Right expr -> do
                    mval <- liftIO $ E.eval expr envBox
                    case mval of
                        Right val -> case val of
                            T.Empty -> return ()
                            _       -> outputStrLn ("=> " ++ show val)
                        Left e -> outputStrLn (show e)
                Left e -> outputStrLn (show e)
        loop envBox

-- Get a list of Scheme Expression String's, and return the corresponding output String's
runStrings :: [String] -> IORef T.Env -> IO [String]
runStrings xs envBox = foldl seedGen (return []) xs  where
    seedGen mseed x = do
        seed <- mseed
        case P.run x of
            Left  err  -> return $ seed ++ [show err]
            Right expr -> do
                val <- E.eval expr envBox
                return $ seed ++ [show val]

-- Get a String of Scheme Expression's, then read and eval all of them
runScheme :: String -> IORef T.Env -> IO (Either T.ScmErr T.Exp)
runScheme str envBox = case P.runList str of
    Left  e  -> return $ Left e
    Right xs -> E.evalList xs envBox
