module Parser
    ( run
    , runList
    ) where
import           Data.Either.Combinators
import           Data.List
import           Text.ParserCombinators.Parsec
import qualified Types                         as T

eatComment :: String -> String
eatComment = unlines . map eatCommentLine . lines
  where
    eatCommentLine =
        unwords . takeWhile (\w -> not (";" `isPrefixOf` w)) . words

symChar :: Parser Char
symChar = oneOf "!#$%&|*+-/:<=>?@^_~"

symbol :: Parser T.Exp
symbol = do
    x  <- letter <|> symChar
    xs <- many (letter <|> digit <|> symChar)
    return $ T.Symbol (x : xs)

-- A naïve string implementation
-- TODO: implement parsing of escape sequences
str :: Parser T.Exp
str = do
    _ <- char '"'
    s <- manyTill anyChar (char '"')
    return $ T.String s

posNumber :: Parser T.Exp
posNumber = do
    x  <- digit <|> char '.'
    xs <- many (digit <|> oneOf ".e-")
    let res = read (x : xs)
    return $ T.Number res

negNumber :: Parser T.Exp
negNumber = do
    _              <- char '-'
    (T.Number res) <- posNumber
    return $ T.Number (negate res)

number :: Parser T.Exp
number = negNumber <|> posNumber

atom :: Parser T.Exp
atom = try number <|> symbol

regList :: Parser T.Exp
regList = do
    char '(' >> skipMany space
    res <- sepEndBy expr (many1 space)
    _   <- char ')'
    return $ T.List res

dottedList :: Parser T.Exp
dottedList = do
    char '(' >> skipMany space
    x <- expr
    skipMany1 space >> char '.' >> skipMany1 space
    y <- expr
    _ <- skipMany space >> char ')'
    return $ T.List [x, y]

list :: Parser T.Exp
list = try dottedList <|> regList

quoted :: Parser T.Exp
quoted = do
    _ <- char '\''
    r <- expr
    return $ T.List [T.Symbol "quote", r]

expr :: Parser T.Exp
expr = choice [str, quoted, list, atom]

exprs :: Parser [T.Exp]
exprs = sepEndBy1 expr $ many1 space

toScmErr :: ParseError -> T.ScmErr
toScmErr = T.ScmErr . show

run :: String -> Either T.ScmErr T.Exp
run = mapLeft toScmErr . parse expr "yascm" . eatComment

runList :: String -> Either T.ScmErr [T.Exp]
runList = mapLeft toScmErr . parse exprs "yascm" . eatComment
