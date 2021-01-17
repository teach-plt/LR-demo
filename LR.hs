{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- | LR-parser.

module Main where

-- import Control.Monad.Except
-- import Control.Monad.State
-- import Control.Monad.Writer (Writer, runWriter, tell)

-- import qualified Data.Foldable as Fold
-- import qualified Data.List as List
-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set

-- import Data.Maybe (mapMaybe)
-- import Data.Monoid (Any(..))
-- import Data.Tuple (swap)

import System.Environment (getArgs)
import System.Exit (exitFailure)

-- -- uses microlens-platform
-- import Lens.Micro
-- import Lens.Micro.Extras (view)
-- import Lens.Micro.TH (makeLenses)

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
