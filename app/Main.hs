module Main where

import qualified ArgParse as Arg
import Data.String.Conversions (cs)
import Options.Applicative
  ( execParser,
    fullDesc,
    header,
    helper,
    info,
  )
import Relude hiding (Text)
import qualified Repl as R
import qualified ScmPrelude as Scm

welcomeBanner :: String
welcomeBanner = "Welcome to yascm, a simple Scheme interpreter."

stdlibPath :: String
stdlibPath = "./scheme/stdlib.rkt"

main :: IO ()
main = dispatch =<< execParser opts
  where
    opts =
      info
        (Arg.args <**> helper)
        ( fullDesc <> header "yascm - Yet Another SCheMe interpreter in Haskell."
        )

dispatch :: Arg.Args -> IO ()
dispatch a = do
  let fin = Arg.fin a
  let repl = Arg.repl a

  {-
  putStrLn
      $  "File: "
      ++ (show $ Arg.fin a)
      ++ ", Interactive: "
      ++ (show $ Arg.repl a)
  -}

  globalEnv <- newIORef Scm.prelude

  let readSourceFile path = do
        contents <- cs <$> readFile path
        void . runExceptT $ R.runScheme contents globalEnv

  let readSourceFileVerbose path = do
        putStr $ ".. Reading `" ++ path ++ "`: "
        contents <- cs <$> readFile path
        res <- runExceptT $ R.runScheme contents globalEnv
        case res of
          Right _ -> putStrLn "Done."
          Left e -> print e

  let runRepl = R.repl globalEnv

  {-
  -- debug
  let checkEnv = do
          e <- readIORef globalEnv
          print (T.dict e)
  -}

  putStrLn welcomeBanner

  -- load stdlib
  readSourceFileVerbose stdlibPath

  case fin of
    Just path ->
      if repl
        then do
          readSourceFileVerbose path
          runRepl
        else readSourceFile path
    Nothing -> runRepl