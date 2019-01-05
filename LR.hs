{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- | LR-parser.

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer (Writer, runWriter, tell)

import qualified Data.Foldable as Fold
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (mapMaybe)
import Data.Monoid (Any(..))
import Data.Tuple (swap)

import System.Environment (getArgs)
import System.Exit (exitFailure)

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

import qualified LBNF.Abs as A
import LBNF.Par (pGrammar, myLexer)
import LBNF.Print (Print, printTree)
import LBNF.ErrM (Err(Ok, Bad))

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

  putStrLn "Using the following grammar:"
  putStrLn $ printTree $ reifyGrammar grm

  let egrm = makeEGrammar grm start
  let ipt  = ptGen egrm

  putStrLn "Generated parse table:"
  putStrLn $ debugPrint ipt

  let pt   = constructParseTable' ipt

  -- Run the parser.
  putStrLn "Parsing stdin..."
  stdin <- getContents
  -- runM $ parseWith pt stdin
  -- putStrLn "Parse successful!"
  print $ runLR1Parser pt stdin

type M = Either String

runM :: M a -> IO a
runM = \case
  Right a -> return a
  Left err -> do
    putStrLn $ "Error: " ++ err
    exitFailure

runErr :: IO () ->  Err a -> IO a
runErr preErr = \case
  Ok a -> return a
  Bad err -> do
    preErr
    putStrLn $ "Error: " ++ err
    exitFailure


-- -- | LR(1) parser.

-- parseWith :: ParseTable -> String -> M ()
-- parseWith grm inp = throwError "NYI: LR parser"
