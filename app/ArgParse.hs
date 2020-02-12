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
                        "Load a source file"
                    )
                )
        <*> switch (long "repl" <> short 'i' <> help "REPL (interactive) mode")
