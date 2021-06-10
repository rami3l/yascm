{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Types
  ( Exp (..),
    ScmErr (..),
    Env (..),
    fromOuter,
    Types.lookup,
    insertValue,
    setValue,
    tryToList,
    isList,
    scmNil,
    toConsCell,
    isDefined,
  )
where

import qualified Data.Map.Lazy as Map
import Data.String.Conversions (cs)
import Data.Text.Format (format)
import Data.Text.Lazy (Text)
import GHC.Base (returnIO)
import Relude hiding (Text, show)
import Text.RawString.QQ (r)
import Prelude (show, unwords)

data Exp
  = ScmBool Bool
  | ScmSym Text
  | ScmStr Text
  | ScmInt Integer
  | ScmDouble Double
  | -- | An unevaluated Scheme list.
    -- Only used as an AST component (eg. when expressing function calls),
    -- does not appear in evaluation results.
    ScmList [Exp]
  | -- | A `Cons` pair made up by two expressions.
    ScmCons
      { car :: Exp,
        cdr :: Exp
      }
  | -- | An anonymous function.
    ScmClosure
      { -- | Should be `ScmList`.
        --
        -- @body := (ScmList (ScmList (vars) : defs))@
        body :: Exp,
        env :: IORef Env
      }
  | ScmPrimitive ([Exp] -> Either ScmErr Exp)

-- | The special class signifying the end of a list.
-- Also used as an empty expression.
scmNil :: Exp
scmNil = ScmList []

toConsCell :: [Exp] -> Exp
toConsCell [] = scmNil
toConsCell (x : xs) = ScmCons x . toConsCell $ xs

tryToList :: Exp -> Maybe [Exp]
tryToList (ScmList l) = Just l
tryToList (ScmCons car' (ScmList [])) = Just [car']
tryToList (ScmCons car' cdr') = tryToList cdr' & fmap (car' :)
tryToList _ = Nothing

isList :: Exp -> Bool
isList = isJust . tryToList

instance Show Exp where
  show (ScmBool b) = if b then "#t" else "#f"
  show (ScmSym s) = s & cs
  show (ScmStr txt) = format [r|"{}"|] [txt] & cs
  show (ScmInt i) = show i
  show (ScmDouble d) = show d
  show (ScmList l) = format "({})" [l & fmap show & Prelude.unwords] & cs
  -- ! TODO: Implement auto conversion from ScmCons to ScmList
  show (ScmCons car' cdr') = format "({} . {})" [show car', show cdr'] & cs
  show (ScmClosure body' _) =
    let (ScmList (ScmList vars : _)) = body'
     in format "<Closure: {}>" [show vars] & cs
  show (ScmPrimitive _) = "<Primitive>"

newtype ScmErr = ScmErr
  { reason :: Text
  }

instance Show ScmErr where
  show = cs . reason

data Env = Env
  { dict :: Map.Map Text Exp,
    outer :: Maybe (IORef Env)
  }

fromOuter :: IORef Env -> Env
fromOuter fromEnvBox = Env Map.empty $ Just fromEnvBox

-- | Find the definition of a symbol.
lookup :: Text -> IORef Env -> MaybeT IO Exp
lookup s envBox = do
  env' <- lift $ readIORef envBox
  case Map.lookup s (dict env') of
    Just def -> return def
    Nothing -> do
      outerEnv <- MaybeT . returnIO . outer $ env'
      Types.lookup s outerEnv

isDefined :: Text -> IORef Env -> IO Bool
isDefined s envBox = isJust <$> runMaybeT (Types.lookup s envBox)

insertValue :: Text -> Exp -> IORef Env -> IO ()
insertValue sym def envBox = do
  (Env d mo) <- readIORef envBox
  writeIORef envBox $ Env (Map.insert sym def d) mo

setValue :: Text -> Exp -> IORef Env -> IO ()
setValue sym def envBox = do
  (Env d mo) <- readIORef envBox
  let isLocal = isJust $ Map.lookup sym d
  isDefined' <- do
    res <- runMaybeT $ Types.lookup sym envBox
    return $ isJust res

  if not isLocal && isDefined'
    then do
      let (Just o) = mo
      setValue sym def o
    else insertValue sym def envBox