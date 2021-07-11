{-# LANGUAGE FlexibleContexts #-}

module Repl
  ( repl,
    runScheme,
    runStrings,
  )
where

import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Writer.CPS (WriterT, tell)
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import qualified EvalApply as E
import qualified Parser as P
import Relude hiding (Text, show)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import qualified Types as T
import Prelude (show)

loop :: IORef T.Env -> InputT IO ()
loop envBox' = do
  let printLn = outputStrLn . show
  line :: Maybe String <- getInputLine ">> "
  whenJust (cs <$> line :: Maybe Text) $ \line' -> case P.run line' of
    Left e -> printLn e
    Right expr -> do
      mval <- lift . runExceptT $ E.eval expr envBox'
      case mval of
        Right (T.ScmList []) -> return ()
        Right val -> outputStrLn $ "=> " ++ show val
        Left e -> printLn e
  loop envBox'

repl :: IORef T.Env -> IO ()
repl envBox = runInputT defaultSettings (loop envBox)

runString :: Text -> IORef T.Env -> WriterT [Text] IO ()
runString x envBox =
  let showEither (Left err) = cs . show $ err
      showEither (Right val) = cs . show $ val
      eval (Left err) = return . cs . show $ err
      eval (Right expr) = do
        val <- runExceptT $ E.eval expr envBox
        return . showEither $ val
   in do
        ln <- lift . eval . P.run $ x
        tell [ln]

-- Get a list of Scheme Expression String's, and return the corresponding output String's
runStrings :: [Text] -> IORef T.Env -> WriterT [Text] IO ()
runStrings xs envBox = forM_ xs (`runString` envBox)

-- Get a String of Scheme Expression's, then read and eval all of them
runScheme :: Text -> IORef T.Env -> ExceptT T.ScmErr IO T.Exp
runScheme str envBox = do
  exprs <- except $ P.runList str
  E.evalList exprs envBox
