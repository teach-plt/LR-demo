{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | LR-parser.

module LR where

import Data.Version                 ( showVersion )

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import qualified LBNF.Abs as A
import LBNF.Par (pGrammar, myLexer)
import LBNF.Print (printTree)

import DebugPrint
import Util

import CFG
import CharacterTokenGrammar
import ParseTable
import ParseTable.Pretty

import License
import qualified Paths_LR_demo as Self ( version )

-- | Main: read file passed by only command line argument and call 'run'.

main :: IO ()
main = do
  getArgs >>= \case
    ["-h"]                -> usage
    ["--help"]            -> usage
    ["-V"]                -> version
    ["--version"]         -> version
    ["--numeric-version"] -> numericVersion
    ["--license"]         -> printLicense
    ['-':_]               -> usage
    [file]                -> run =<< readFile file
    _                     -> usage
  where
    ver = showVersion Self.version
    versionLine = unwords [ "lr-demo version", ver, "(C) 2019-24 Andreas Abel" ]
    usage = do
      putStr $ unlines
        [ versionLine
        , "Call patterns:"
        , "  -h | --help        Print this help text."
        , "  -V | --version     Print version info."
        , "  --numeric-version  Print just the version number."
        , "  --license          Print the license text."
        , "  FILE               Parses stdin with the LBNF grammar given in FILE."
        ]
      exitFailure
    version = do
      putStr $ unlines
        [ versionLine
        , "Developed for the course Programming Language Technology;"
        , "Chalmers DAT151 / University of Gothenburg DIT231."
        , "License: BSD 3-clause."
        ]
    numericVersion = do
      putStrLn ver
      exitSuccess
    printLicense = do
      putStr license
      exitSuccess

-- | Parse grammar and then use it to parse stdin.

run :: String -> IO ()
run s = do

  -- Parse CFG grammar from file in LBNF syntax
  tree <- runErr (putStrLn "Syntax error in grammar file") $
    pGrammar (myLexer s)

  -- Scope-check grammar and convert into internal format.
  (mstart, grm)  <- runM $ checkGrammar tree

  start <- runM $ case mstart of
    Just start -> Right start
    Nothing    -> Left "grammar is empty!"

  putStrLn $ unlines
    [ "Using the following grammar:"
    , ""
    , printTree $ reifyGrammar grm
    ]

  let newstart = A.Ident "%start"
  let egrm = addNewStart newstart newstart $ makeEGrammar grm start
  let ipt  = ptGen egrm

  putStrLn $ unlines
    [ "Generated parse table:"
    , ""
    , debugPrint $ WithNTNames @A.Ident (getNTNames egrm) ipt
    ]

  let pt   = constructParseTable' ipt

  -- Run the parser.
  putStrLn "Parsing stdin..."
  stdin <- trim <$> getContents
  -- runM $ parseWith pt stdin
  -- putStrLn "Parse successful!"
  putStrLn $ debugPrint $ runLR1Parser pt stdin

type Err = Either String

runM :: Err a -> IO a
runM = runErr $ return ()

runErr :: IO () -> Err a -> IO a
runErr preErr = \case
  Right a -> return a
  Left err -> do
    preErr
    putStrLn $ "Error: " ++ err
    exitFailure
