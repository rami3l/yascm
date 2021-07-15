module ArgParse
  ( Args (..),
    args,
  )
where

import Options.Applicative
  ( Parser,
    help,
    long,
    metavar,
    short,
    strOption,
    switch,
  )
import Relude hiding (Text)

data Args = Args
  { fin :: Maybe String,
    repl :: Bool
  }

args :: Parser Args
args =
  let load =
        strOption $
          long "load" <> short 'f' <> metavar "FILE"
            <> help "Load a source file"
      repl = long "repl" <> short 'i' <> help "REPL (interactive) mode"
   in Args
        <$> optional load
        <*> switch repl
