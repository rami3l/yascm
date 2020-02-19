module ScmPrelude
    ( prelude
    )
where

import           Types
import qualified Data.Map                      as Map


prelude :: Env
prelude =
    let o = Nothing
        d = Map.fromList
            [ ("+"   , makePrim add)
            , ("-"   , makePrim sub)
            , ("*"   , makePrim mul)
            , ("/"   , makePrim ScmPrelude.div)
            , ("="   , makePrim eq)
            , ("<"   , makePrim lt)
            , ("<="  , makePrim le)
            , (">"   , makePrim gt)
            , (">="  , makePrim ge)
            , ("car" , makePrim car)
            , ("cdr" , makePrim cdr)
            , ("cons", makePrim cons)
            , ("list", makePrim list)
            , ( "null?"
              , makePrim isNull
              )
            -- , ("display", makePrim display)
            -- , ("newline", makePrim newline)
            -- , ("exit", makePrim exit)
            , ("#t"  , Boolean True)
            , ("#f"  , Boolean False)
            , ("null", List [])
            ]
    in  Env d o

sigma :: (Exp -> Exp -> Either ScmErr Exp) -> [Exp] -> Either ScmErr Exp
sigma helper xs = case xs of
    [x, y] -> helper x y
    y : ys -> (helper y) =<< sigma helper ys
    _      -> Left $ ScmErr $ "add/mul : expected multiple terms"

add :: [Exp] -> Either ScmErr Exp
add = sigma helper  where
    helper (Number x) (Number y) = Right $ Number (x + y)
    helper _          _          = Left $ ScmErr $ "add: expected Number"

sub :: [Exp] -> Either ScmErr Exp
sub [(Number x), (Number y)] = Right $ Number (x - y)
sub _                        = Left $ ScmErr $ "sub: expected Number"

mul :: [Exp] -> Either ScmErr Exp
mul = sigma helper  where
    helper (Number x) (Number y) = Right $ Number (x * y)
    helper _          _          = Left $ ScmErr $ "mul: expected Number"

div :: [Exp] -> Either ScmErr Exp
div [(Number x), (Number y)] = if y /= 0
    then Right $ Number (x / y)
    else Left $ ScmErr $ "div: divided by 0"
div _ = Left $ ScmErr $ "div: expected Number"

eq :: [Exp] -> Either ScmErr Exp
eq [(Number  x), (Number y) ] = Right $ Boolean (x == y)
eq [(Boolean x), (Boolean y)] = Right $ Boolean (x == y)
eq _                          = Left $ ScmErr $ "eq: expected Number or Boolean"

lt :: [Exp] -> Either ScmErr Exp
lt [(Number  x), (Number y) ] = Right $ Boolean (x < y)
lt [(Boolean x), (Boolean y)] = Right $ Boolean (x < y)
lt _                          = Left $ ScmErr $ "lt: expected Number or Boolean"

le :: [Exp] -> Either ScmErr Exp
le [(Number  x), (Number y) ] = Right $ Boolean (x <= y)
le [(Boolean x), (Boolean y)] = Right $ Boolean (x <= y)
le _                          = Left $ ScmErr $ "le: expected Number or Boolean"

gt :: [Exp] -> Either ScmErr Exp
gt [(Number  x), (Number y) ] = Right $ Boolean (x > y)
gt [(Boolean x), (Boolean y)] = Right $ Boolean (x > y)
gt _                          = Left $ ScmErr $ "gt: expected Number or Boolean"

ge :: [Exp] -> Either ScmErr Exp
ge [(Number  x), (Number y) ] = Right $ Boolean (x >= y)
ge [(Boolean x), (Boolean y)] = Right $ Boolean (x >= y)
ge _                          = Left $ ScmErr $ "ge: expected Number or Boolean"

car :: [Exp] -> Either ScmErr Exp
car [List (x : xs)] = Right x
car _               = Left $ ScmErr $ "car: expected a List"

cdr :: [Exp] -> Either ScmErr Exp
cdr [List (x : xs)] = Right $ List xs
cdr _               = Left $ ScmErr $ "cdr: expected a List"

cons :: [Exp] -> Either ScmErr Exp
cons [x, (List y)] = Right $ List (x : y)
cons _             = Left $ ScmErr $ "cons: expected an Exp and a List"

list :: [Exp] -> Either ScmErr Exp
list xs = Right $ List xs

isNull :: [Exp] -> Either ScmErr Exp
isNull [List []] = Right $ Boolean True
isNull [List _ ] = Right $ Boolean False
isNull _         = Left $ ScmErr $ "null?: expected a List"

