module ScmPrelude
  ( prelude,
  )
where

import qualified Data.Map as Map
import Data.Text.Format (format)
import Data.Text.Lazy (Text)
import Relude hiding (Text)
import Types as T
  ( Env (..),
    Exp (ScmBool, ScmCons, ScmDouble, ScmInt, ScmList, ScmPrimitive),
    ScmErr (..),
    toConsCell,
  )

prelude :: Env
prelude =
  Env
    { outer = Nothing,
      dict =
        Map.fromList $
          second ScmPrimitive
            <$> [ ("+", add),
                  ("-", sub),
                  ("*", mul),
                  ("/", ScmPrelude.div),
                  ("=", eq),
                  ("<", lt),
                  ("<=", le),
                  (">", gt),
                  (">=", ge),
                  ("car", car),
                  ("cdr", cdr),
                  ("cons", cons),
                  ("list", list),
                  ("nil?", isNil),
                  ("boolean?", isBoolean)
                ]
    }

car :: [Exp] -> Either ScmErr Exp
car [ScmCons x _] = Right x
car _ = Left $ ScmErr "car: expected a Cons"

cdr :: [Exp] -> Either ScmErr Exp
cdr [ScmCons _ y] = Right y
cdr _ = Left $ ScmErr "cdr: expected a Cons"

cons :: [Exp] -> Either ScmErr Exp
cons [x, y] = Right $ ScmCons x y
cons _ = Left $ ScmErr "cons: expected 2 expressions"

list :: [Exp] -> Either ScmErr Exp
list = Right . toConsCell

isNil :: [Exp] -> Either ScmErr Exp
isNil [ScmList []] = Right $ ScmBool True
isNil _ = Right $ ScmBool False

isBoolean :: [Exp] -> Either ScmErr Exp
isBoolean [ScmBool _] = Right $ ScmBool True
isBoolean _ = Right $ ScmBool False

sigma :: (Exp -> Exp -> Either ScmErr Exp) -> [Exp] -> Either ScmErr Exp
sigma helper xs = case xs of
  [x, y] -> helper x y
  y : ys -> helper y =<< sigma helper ys
  _ -> Left $ ScmErr "add/mul : expected multiple terms"

add2 :: (Double -> Double -> Double) -> Text -> Exp -> Exp -> Either ScmErr Exp
-- ! Hack: Redundant conversion.
add2 op _ (ScmInt x) (ScmInt y) = Right . ScmInt . truncate $ fromIntegral x `op` fromIntegral y
add2 op _ (ScmDouble x) (ScmInt y) = Right . ScmDouble $ x `op` fromIntegral y
add2 op _ (ScmInt x) (ScmDouble y) = Right . ScmDouble $ fromIntegral x `op` y
add2 op _ (ScmDouble x) (ScmDouble y) = Right . ScmDouble $ x `op` y
add2 _ opName _ _ = Left . ScmErr $ format "{}: expected Numbers" [opName]

add :: [Exp] -> Either ScmErr Exp
add = sigma $ add2 (+) "add"

sub :: [Exp] -> Either ScmErr Exp
sub [x, y] = add2 (-) "sub" x y
sub _ = Left $ ScmErr "sub: expected Numbers"

mul :: [Exp] -> Either ScmErr Exp
mul = sigma $ add2 (*) "mul"

divider :: Double -> Double -> Either ScmErr Exp
divider x y = if y /= 0 then Right . ScmDouble $ x / y else Left $ ScmErr "div: divided by 0"

div :: [Exp] -> Either ScmErr Exp
div [ScmInt x, ScmInt y] = fromIntegral x `divider` fromIntegral y
div [ScmDouble x, ScmInt y] = x `divider` fromIntegral y
div [ScmInt x, ScmDouble y] = fromIntegral x `divider` y
div [ScmDouble x, ScmDouble y] = divider x y
div _ = Left $ ScmErr "div: expected Number"

-- | Convert a bool to a `Num`.
fromBool :: Num a => Bool -> a
fromBool True = 1
fromBool False = 0

comp2 :: (Double -> Double -> Bool) -> Text -> [Exp] -> Either ScmErr Exp
comp2 op _ [ScmInt x, ScmInt y] = Right . ScmBool $ fromIntegral x `op` fromIntegral y
comp2 op _ [ScmDouble x, ScmInt y] = Right . ScmBool $ x `op` fromIntegral y
comp2 op _ [ScmInt x, ScmDouble y] = Right . ScmBool $ fromIntegral x `op` y
comp2 op _ [ScmDouble x, ScmDouble y] = Right . ScmBool $ x `op` y
comp2 op _ [ScmBool x, ScmBool y] = Right . ScmBool $ fromBool x `op` fromBool y
comp2 _ opName _ = Left . ScmErr $ format "{}: expected Numbers or Booleans" [opName]

eq :: [Exp] -> Either ScmErr Exp
eq = comp2 (==) "eq"

lt :: [Exp] -> Either ScmErr Exp
lt = comp2 (<) "lt"

le :: [Exp] -> Either ScmErr Exp
le = comp2 (<=) "le"

gt :: [Exp] -> Either ScmErr Exp
gt = comp2 (>) "gt"

ge :: [Exp] -> Either ScmErr Exp
ge = comp2 (>=) "ge"
