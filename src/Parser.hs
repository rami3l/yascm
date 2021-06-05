{-# LANGUAGE OverloadedStrings #-}

module Parser
  (
  )
where

-- run,
-- runList,

import Data.Either.Combinators (mapLeft)
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.String.Conversions (cs)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Read (rational, signed)
import Text.Parsec
  ( ParseError,
    alphaNum,
    char,
    choice,
    letter,
    many,
    many1,
    oneOf,
    parse,
    sepEndBy,
    sepEndBy1,
    skipMany,
    skipMany1,
    space,
    try,
    (<|>),
  )
import Text.Parsec.Text.Lazy (Parser)
import Text.Parsec.Token
  ( GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
  )
import qualified Types as T

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
      opStart = symChar,
      opLetter = alphaNum <|> symChar,
      reservedNames = [],
      reservedOpNames = [],
      caseSensitive = True
    }

scmLexer :: GenTokenParser Text () Identity
scmLexer = makeTokenParser scmDef

sym :: Parser T.Exp
sym = T.ScmSym . cs <$> (identifier scmLexer <|> operator scmLexer)

str :: Parser T.Exp
str = T.ScmStr . cs <$> stringLiteral scmLexer

int :: Parser T.Exp
int = T.ScmInt <$> integer scmLexer

double :: Parser T.Exp
double = T.ScmDouble <$> float scmLexer

atom :: Parser T.Exp
atom = try (int <|> double) <|> sym

regList :: Parser T.Exp
regList = parens scmLexer $ T.ScmList <$> many expr

dottedList :: Parser T.Exp
dottedList =
  parens
    scmLexer
    ( do
        car <- expr
        _ <- dot scmLexer
        T.ScmCons car <$> expr
    )

list :: Parser T.Exp
list = try dottedList <|> regList

quoted :: Parser T.Exp
quoted = do
  _ <- char '\''
  r <- expr
  return $ T.ScmList [T.ScmSym "quote", r]

expr :: Parser T.Exp
expr = choice [str, quoted, list, atom]

toScmErr :: ParseError -> T.ScmErr
toScmErr = T.ScmErr . cs . show

run :: Text -> Either T.ScmErr T.Exp
run = mapLeft toScmErr . parse expr "yascm"

runList :: Text -> Either T.ScmErr [T.Exp]
runList = mapLeft toScmErr . parse (many1 expr) "yascm"