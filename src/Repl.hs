{-# LANGUAGE FlexibleContexts #-}

module Repl
  ( repl,
    runScheme,
    runStrings,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Function ((&))
import Data.IORef (IORef)
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import qualified EvalApply as E
import qualified Parser as P
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import qualified Types as T
import Prelude hiding (Text)

loop :: IORef T.Env -> InputT IO ()
loop envBox' = do
  mLine <- getInputLine ">> "
  case cs <$> mLine of
    Nothing -> return ()
    Just line -> case P.run line of
      Left e -> outputStrLn . show $ e
      Right expr -> do
        mval <- lift . runExceptT $ E.eval expr envBox'
        case mval of
          Right (T.ScmList []) -> return ()
          Right val -> outputStrLn $ "=> " ++ show val
          Left e -> outputStrLn $ show e
  loop envBox'

repl :: IORef T.Env -> IO ()
repl envBox = runInputT defaultSettings (loop envBox)

-- Get a list of Scheme Expression String's, and return the corresponding output String's
runStrings :: [Text] -> IORef T.Env -> IO [Text]
runStrings xs envBox = foldl seedGen (return []) xs
  where
    seedGen mSeed x = do
      seed <- mSeed
      P.run x
        & either
          (\err -> return $ seed ++ [cs $ show err])
          ( \expr -> do
              val <- runExceptT $ E.eval expr envBox
              return $ seed ++ [cs $ show val]
          )

-- Get a String of Scheme Expression's, then read and eval all of them
runScheme :: Text -> IORef T.Env -> ExceptT T.ScmErr IO T.Exp
runScheme str envBox = do
  exprs <- except $ P.runList str
  E.evalList exprs envBox
