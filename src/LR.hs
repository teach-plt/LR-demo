{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | LR-parser.

module LR where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified LBNF.Abs as A
import LBNF.Par (pGrammar, myLexer)
import LBNF.Print (printTree)

import DebugPrint
import Util

import CFG
import CharacterTokenGrammar
import ParseTable
import ParseTable.Pretty

-- | Main: read file passed by only command line argument and call 'run'.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> run =<< readFile file
    _      -> do
      putStrLn "Usage: LR <file.cf>"
      putStrLn "Parses stdin with the grammar given in the LBNF <file.cf>"
      exitFailure

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


-- -- | LR(1) parser.

-- parseWith :: ParseTable -> String -> M ()
-- parseWith grm inp = throwError "NYI: LR parser"

-- type Input = Input' Term

-- instance DebugPrint Input where
--   debugPrint = id
