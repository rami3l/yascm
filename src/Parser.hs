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
    letter,
    many,
    many1,
    oneOf,
    parse,
    space,
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

plus :: Parser T.Exp
plus = do
  _ <- char '+' >> many1 space
  return $ T.ScmSym "+"

minus :: Parser T.Exp
minus = do
  _ <- char '-' >> many1 space
  return $ T.ScmSym "-"

atom :: Parser T.Exp
atom = choice $ try <$> [plus, minus, double, int, sym]

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
runList = mapLeft toScmErr . parse (whiteSpace scmLexer >> many1 expr) "yascm"