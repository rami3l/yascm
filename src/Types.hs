module Types
    ( Exp(..)
    , ScmPrimitive(..)
    , makePrim
    , ScmClosure(..)
    , ScmErr(..)
    , Env(..)
    , fromOuter
    , Types.lookup
    , setValue
    )
where
import qualified Data.Map                      as Map

data Exp = Boolean Bool
         | Symbol String
         | Number Double
         | List [Exp]
         | Closure ScmClosure
         | Primitive ScmPrimitive
         | Empty

instance Show Exp where
    show (Boolean   b) = show b
    show (Symbol    s) = s
    show (Number    n) = show n
    show (List      l) = show l
    show (Closure   c) = show c
    show (Primitive p) = show p
    show Empty         = ""

newtype ScmPrimitive = ScmPrimitive ([Exp] -> Either ScmErr Exp)

instance Show ScmPrimitive where
    show prim = "<Primitive>"

makePrim :: ([Exp] -> Either ScmErr Exp) -> Exp
makePrim = Primitive . ScmPrimitive

data ScmClosure = ScmClosure {
    body :: Exp,
    env :: Env
}

instance Show ScmClosure where
    show clos = "<Closure>"

newtype ScmErr = ScmErr {
    reason :: String
}

instance Show ScmErr where
    show = reason

data Env = Env {
    dict :: Map.Map String Exp,
    outer :: Maybe Env
}

fromOuter :: Env -> Env
fromOuter fromEnv = Env Map.empty (Just fromEnv)

{-| Find the definition of a Symbol -}
lookup :: String -> Env -> Maybe Exp
lookup s env = case Map.lookup s (dict env) of
    Just def -> Just def
    Nothing  -> case outer env of
        Just o  -> Types.lookup s o
        Nothing -> Nothing

setValue :: String -> Exp -> Env -> Env
setValue sym def (Env d Nothing ) = Env (Map.insert sym def d) Nothing
setValue sym def (Env d (Just o)) = Env d $ Just (setValue sym def o)
