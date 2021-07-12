module Parser
  ( run,
    runList,
  )
where

import Data.Either.Combinators (mapLeft)
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import Relude hiding (Text, many, show)
import Text.Parsec
  ( ParseError,
    alphaNum,
    char,
    choice,
    digit,
    letter,
    many,
    many1,
    notFollowedBy,
    oneOf,
    optionMaybe,
    parse,
    try,
  )
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token
  ( GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
  )
import qualified Types as T
import Prelude (show)

symChar :: Parser Char
symChar = oneOf "!#$%&|*+-/:<=>?@^_~"

scmDef :: GenLanguageDef Text () Identity
scmDef =
  LanguageDef
    { commentStart = "#|",
      commentEnd = "|#",
      commentLine = ";",
      nestedComments = True,
      identStart = letter,
      identLetter = alphaNum <|> symChar,
      opStart = digit <|> symChar,
      opLetter = alphaNum <|> symChar,
      reservedNames = [],
      reservedOpNames = [],
      caseSensitive = True
    }

scmLexer :: GenTokenParser Text () Identity
scmLexer = makeTokenParser scmDef

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace scmLexer

ident :: Parser T.Exp
ident = T.ScmSym . cs <$> identifier scmLexer

op :: Parser T.Exp
op = T.ScmSym . cs <$> operator scmLexer

str :: Parser T.Exp
str = T.ScmStr . cs <$> stringLiteral scmLexer

positiveIntOrDouble :: Parser T.Exp
positiveIntOrDouble = do
  num <- naturalOrFloat scmLexer
  notFollowedBy symChar
  return $ either T.ScmInt T.ScmDouble num

intOrDouble :: Parser T.Exp
intOrDouble =
  let negate' sx = case sx of
        T.ScmInt x -> T.ScmInt $ negate x
        T.ScmDouble x -> T.ScmDouble $ negate x
        -- Safety: this case will not be encountered,
        -- due to the definition of `positiveIntOrDouble`.
        _ -> T.scmNil
   in do
        sign <- optionMaybe $ oneOf "+-"
        case sign of
          Nothing -> positiveIntOrDouble
          Just '+' -> positiveIntOrDouble
          Just '-' -> negate' <$> positiveIntOrDouble
          -- Safety: this case will not be encountered,
          -- due to the definition of `oneOf`.
          _ -> return T.scmNil

atom :: Parser T.Exp
atom = choice [try intOrDouble, try op, ident]

inParens :: Parser T.Exp -> Parser T.Exp
inParens = parens scmLexer

regList :: Parser T.Exp
regList = inParens $ T.ScmList <$> many expr

dottedList :: Parser T.Exp
dottedList = inParens $ do
  car <- expr
  T.ScmCons car <$> (dot scmLexer >> expr)

list :: Parser T.Exp
list = try dottedList <|> regList

quoted :: Parser T.Exp
quoted = do
  r <- char '\'' >> expr
  return $ T.ScmList [T.ScmSym "quote", r]

expr :: Parser T.Exp
expr = choice [str, quoted, list, atom]

toScmErr :: ParseError -> T.ScmErr
toScmErr = T.ScmErr . cs . show

run :: Text -> Either T.ScmErr T.Exp
run = mapLeft toScmErr . parse expr "yascm"

runList :: Text -> Either T.ScmErr [T.Exp]
runList = mapLeft toScmErr . parse (whiteSpace' >> many1 expr) "yascm"