{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Function ((&))
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Lazy as Map
import Data.Maybe (isJust)
import Data.String.Conversions (cs)
import Data.Text.Format (format)
import Data.Text.Lazy (Text)
import GHC.Base (returnIO)
import Text.RawString.QQ (r)

data Exp
  = ScmBool Bool
  | ScmSym Text
  | ScmStr Text
  | ScmInt Integer
  | ScmDouble Double
  | ScmList [Exp]
  | ScmCons
      { car :: Exp,
        cdr :: Exp
      }
  | ScmClosure
      { -- | Should be @ScmList@.
        --
        -- @body := (ScmList (ScmList (vars) : defs))@
        body :: Exp,
        env :: IORef Env
      }
  | ScmPrimitive ([Exp] -> Either ScmErr Exp)

scmNil :: Exp
scmNil = ScmList []

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

insertValue :: Text -> Exp -> IORef Env -> IO ()
insertValue sym def envBox = do
  (Env d mo) <- readIORef envBox
  writeIORef envBox $ Env (Map.insert sym def d) mo

setValue :: Text -> Exp -> IORef Env -> IO ()
setValue sym def envBox = do
  (Env d mo) <- readIORef envBox
  let isLocal = isJust $ Map.lookup sym d
  isDefined <- do
    res <- runMaybeT $ Types.lookup sym envBox
    return $ isJust res

  if not isLocal && isDefined
    then do
      let (Just o) = mo
      setValue sym def o
    else insertValue sym def envBox