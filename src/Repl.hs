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
import           Data.Function

repl :: IORef T.Env -> IO ()
repl envBox = runInputT defaultSettings (loop envBox)
  where
    loop :: IORef T.Env -> InputT IO ()
    loop envBox' = do
        mLine <- getInputLine ">> "
        mLine & maybe
            (return ())
            (\line -> P.run line & either
                (outputStrLn . show)
                (\expr -> do
                    mval <- liftIO $ E.eval expr envBox'
                    case mval of
                        Right (T.Empty) -> return ()
                        Right val       -> outputStrLn $ "=> " ++ show val
                        Left  e         -> outputStrLn $ show e
                )
            )
        loop envBox'

-- Get a list of Scheme Expression String's, and return the corresponding output String's
runStrings :: [String] -> IORef T.Env -> IO [String]
runStrings xs envBox = foldl seedGen (return []) xs  where
    seedGen mSeed x = do
        seed <- mSeed
        P.run x & either
            (\err -> return $ seed ++ [show err])
            (\expr -> do
                val <- E.eval expr envBox
                return $ seed ++ [show val]
            )

-- Get a String of Scheme Expression's, then read and eval all of them
runScheme :: String -> IORef T.Env -> IO (Either T.ScmErr T.Exp)
runScheme str envBox =
    P.runList str & either (return . Left) (\xs -> E.evalList xs envBox)
