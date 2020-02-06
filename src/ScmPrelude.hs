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
            [ ("+", makePrim add)
            , ("-", makePrim sub)
            , ("*", makePrim mul)
            , ("/", makePrim ScmPrelude.div)
            {-
            , ("="   , makePrim eq)
            , ("<"   , makePrim lt)
            , ("<="  , makePrim le)
            , (">"   , makePrim gt)
            , (">="  , makePrim ge)
            , ("car" , makePrim car)
            , ("cdr" , makePrim cdr)
            , ("cons", makePrim cons)
            , ( "null?"
              , makePrim isNull
              )
            -- , ("display", makePrim display)
            -- , ("newline", makePrim newline)
            -- , ("exit", makePrim exit)
            , ("#t"  , Boolean True)
            , ("#f"  , Boolean False)
            , ("null", List [])
            -}
            ]
    in  Env d o

sigma :: (Exp -> Exp -> Either ScmErr Exp) -> [Exp] -> Either ScmErr Exp
sigma helper xs = case xs of
    [x, y] -> helper x y
    y : ys -> (helper y) =<< add ys

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
