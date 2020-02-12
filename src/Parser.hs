module Parser
    ( run
    , runList
    )
where
import           Text.ParserCombinators.Parsec
import           Data.List
import qualified Types                         as T

eatComment :: String -> String
eatComment = unlines . map eatCommentLine . lines
  where
    eatCommentLine ln =
        let ws = words ln
        in  unwords $ takeWhile (\w -> not (";" `isPrefixOf` w)) ws

symChar :: Parser Char
symChar = oneOf "!#$%&|*+-/:<=>?@^_~"

symbol :: Parser T.Exp
symbol = do
    x  <- letter <|> symChar
    xs <- many (letter <|> digit <|> symChar)
    return $ T.Symbol (x : xs)

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
    res <- sepEndBy expression (many1 space)
    _   <- char ')'
    return $ T.List res

dottedList :: Parser T.Exp
dottedList = do
    char '(' >> skipMany space
    x <- expression
    skipMany1 space >> char '.' >> skipMany1 space
    y <- expression
    _ <- skipMany space >> char ')'
    return $ T.List [x, y]

list :: Parser T.Exp
list = try dottedList <|> regList

quoted :: Parser T.Exp
quoted = do
    _ <- char '\''
    r <- expression
    return $ T.List [T.Symbol "quote", r]

comment :: Parser ()
comment = do
    _ <- char ';'
    _ <- manyTill anyChar newline
    -- !^  This made the comment lines at the end of a string to be a failure
    -- * Workaround: add a new line after the final comment
    spaces
    return ()

expression :: Parser T.Exp
expression = quoted <|> list <|> atom

expressions :: Parser [T.Exp]
expressions = sepEndBy1 expression $ many1 space

toScmErr :: ParseError -> T.ScmErr
toScmErr = T.ScmErr . show

run :: String -> Either T.ScmErr T.Exp
run input = case parse expression "yascm" (eatComment input) of
    Right r -> Right r
    Left  e -> Left $ toScmErr e

runList :: String -> Either T.ScmErr [T.Exp]
runList input = case parse expressions "yascm" (eatComment input) of
    Right r -> Right r
    Left  e -> Left $ toScmErr e
