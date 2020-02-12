module ArgParse
    ( Args(..)
    , args
    )
where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data Args = Args {
    inputFile :: Maybe String,
    repl :: Bool
}

args :: Parser Args
args =
    Args
        <$> optional
                (strOption
                    (long "load" <> short 'f' <> metavar "FILE" <> help
                        "Source file to load"
                    )
                )
        <*> switch (long "repl" <> short 'i' <> help "REPL (interactive) mode")
