module Parser
    ( run
    )
where
import           Text.ParserCombinators.Parsec
import qualified Types                         as T

symChar :: Parser Char
symChar = oneOf "!#$%&|*+-/:<=>?@^_~"

symbol :: Parser T.Exp
symbol = do
    x  <- letter <|> symChar
    xs <- many (letter <|> digit <|> symChar)
    return $ T.Symbol (x : xs)

number :: Parser T.Exp
number = do
    xs <- many1 (digit <|> oneOf ".-")
    let res = read xs
    return $ T.Number res

atom :: Parser T.Exp
atom = number <|> symbol

regList :: Parser T.Exp
regList = do
    char '(' >> skipMany space
    res <- sepEndBy expression (many1 space)
    char ')'
    return $ T.List res

dottedList :: Parser T.Exp
dottedList = do
    char '('
    skipMany space
    x <- expression
    skipMany1 space >> char '.' >> skipMany1 space
    y <- expression
    skipMany space
    char ')'
    return $ T.List [x, y]

list :: Parser T.Exp
list = try dottedList <|> regList

quoted :: Parser T.Exp
quoted = do
    char '\''
    r <- expression
    return $ T.List [T.Symbol "quote", r]

comment :: Parser ()
comment = do
    char ';'
    manyTill anyChar newline
    spaces
    return ()

expression :: Parser T.Exp
expression = do
    skipMany comment
    quoted <|> list <|> atom

toScmErr :: ParseError -> T.ScmErr
toScmErr = T.ScmErr . show

run :: String -> Either T.ScmErr T.Exp
run input = case parse expression "scheme" input of
    Right r -> Right r
    Left  e -> Left $ toScmErr e
