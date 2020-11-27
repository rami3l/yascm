module Types
    ( Exp(..)
    , ScmPrimitive(..)
    , makePrim
    , ScmClosure(..)
    , ScmErr(..)
    , Env(..)
    , fromOuter
    , Types.lookup
    , insertValue
    , setValue
    ) where
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           GHC.Base
import qualified Data.Map                      as Map
import           Data.IORef
import           Data.Maybe

data Exp = Boolean Bool
         | Symbol String
         | String String
         | Number Double
         | List [Exp]
         | Closure ScmClosure
         | Primitive ScmPrimitive
         | Empty

instance Show Exp where
    show (Boolean   b) = show b
    show (Symbol    s) = s
    show (String    s) = "\"" ++ s ++ "\""
    show (Number    n) = show n
    show (List      l) = show l
    show (Closure   c) = show c
    show (Primitive p) = show p
    show Empty         = ""

newtype ScmPrimitive = ScmPrimitive ([Exp] -> Either ScmErr Exp)

instance Show ScmPrimitive where
    show _ = "<Primitive>"

makePrim :: ([Exp] -> Either ScmErr Exp) -> Exp
makePrim = Primitive . ScmPrimitive

data ScmClosure = ScmClosure
    {
    -- body := (List (List (vars) : defs))
      body :: Exp
    , env  :: IORef Env
    }

instance Show ScmClosure where
    show (ScmClosure body' _) =
        let (List (List vars : _)) = body' in "<Closure: " ++ show vars ++ ">"

newtype ScmErr = ScmErr
    { reason :: String
    }

instance Show ScmErr where
    show = reason

data Env = Env
    { dict  :: Map.Map String Exp
    , outer :: Maybe (IORef Env)
    }

fromOuter :: IORef Env -> Env
fromOuter fromEnvBox = Env Map.empty $ Just fromEnvBox

{-| Find the definition of a Symbol -}
lookup :: String -> IORef Env -> MaybeT IO Exp
lookup s envBox = do
    env' <- lift $ readIORef envBox
    case Map.lookup s (dict env') of
        Just def -> return def
        Nothing  -> do
            outerEnv <- MaybeT . returnIO . outer $ env'
            Types.lookup s outerEnv

insertValue :: String -> Exp -> IORef Env -> IO ()
insertValue sym def envBox = do
    (Env d mo) <- readIORef envBox
    writeIORef envBox $ Env (Map.insert sym def d) mo

setValue :: String -> Exp -> IORef Env -> IO ()
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
