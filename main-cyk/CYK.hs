{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CYK where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer (Writer, runWriter, tell)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (mapMaybe)
import Data.Monoid (Any(..))

import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified LBNF.Abs as A
import LBNF.Par (pGrammar, myLexer)
import LBNF.Print (Print, printTree)

import CFG
import CharacterTokenGrammar


-- | Main: read file passed by only command line argument and call 'run'.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> run =<< readFile file
    _      -> do
      putStrLn "Usage: CYK <file.cf>"
      putStrLn "Parses stdin with the grammar given in the LBNF <file.cf>"
      exitFailure

-- | Parse grammar and then use it to parse stdin.

run :: String -> IO ()
run s = do

  -- Parse CFG grammar from file in LBNF syntax
  tree <- runErr (putStrLn "Syntax error in grammar file") $
    pGrammar (myLexer s)

  -- Scope-check grammar and convert into internal format.
  grm  <- snd <$> do runM $ checkGrammar tree

  putStrLn "Using the following grammar:"
  putStrLn $ printTree $ reifyGrammar grm

  -- Do some extra analyses on the grammar.
  let nullability = computeNullable grm
  reportNullable grm nullability

  runM $ checkGuardedness grm

  -- Run the parser.
  putStrLn "Parsing stdin..."
  stdin <- getContents
  runM $ parseWith grm stdin
  putStrLn "Parse successful!"

type Err = Either String

runM :: Err a -> IO a
runM = runErr $ return ()

runErr :: IO () ->  Err a -> IO a
runErr preErr = \case
  Right a -> return a
  Left err -> do
    preErr
    putStrLn $ "Error: " ++ err
    exitFailure

-- | Guardedness: An analysis checking for unproductive cycles like
--   S → S
--   or
--   A → B, B → A.

checkGuardedness :: Grammar -> Err ()
checkGuardedness grm@(Grammar n dict defs) = do
  let gs = computeGuardedness grm
  unless (all getGuarded gs) $ do
    let is = mapMaybe (\ (i, g) -> if getGuarded g then Nothing else Just i) $ IntMap.toList gs
    let us = map show is -- FIXME: map (printTree . ntToIdent grm) is
    throwError $ "ungarded non-terminals in grammar: " ++ unwords us

reportNullable :: Grammar -> IntMap Nullable -> IO ()
reportNullable grm nullability = do
  let ns = map show -- FIXME: map (printTree . ntToIdent grm)
         $ mapMaybe (\ (i, Nullable n) -> if n then Just i else Nothing)
         $ IntMap.toList nullability
  unless (null ns) $
    putStrLn $ "Nullable non-terminals: " ++ unwords ns

{-
-- | Guardedness checking.  Make sure there are no non-productive cycles like
--   @S → S@ or @A → B; B → A@.
--
checkGuardedness :: Grammar -> M ()
checkGuardedness grm@(Grammar n dict defs) = do
  -- Initial state: all NTs are considered unguarded
  let init  = IntMap.map (False,) defs
  let final = saturate (\ gs -> IntMap.traverseWithKey (step gs) gs) init
  unless (all fst final) $ do
    let is = mapMaybe (\ (i, (g , _)) -> if g then Nothing else Just i) $ IntMap.toList final
    let us = map (printTree . ntToIdent grm) is
    throwError $ "ungarded non-terminals in grammar: " ++ unwords us
  where
  step :: IntMap (Bool, NTDef) -> Int -> (Bool, NTDef) -> Change (Bool, NTDef)
  step gs i d@(g, def) = case g of
    False | guarded ntGuarded def -> do
      dirty  -- change!
      return (True, def)
    _ -> return d  -- no change
    where
    ntGuarded j = fst $ IntMap.findWithDefault (error "ntGuarded") j gs

-- | Given guardedness of non-terminals, is a thing guarded?

class Guarded a where
  guarded :: (NT -> Bool) -> a -> Bool

instance Guarded NT where
  guarded gs i = gs i

instance Guarded (Symbol' a) where
  guarded gs = \case
    Term _ -> True
    NT i   -> guarded gs i

-- | Empty productions are guarded.
instance Guarded (Form' a) where
  guarded gs (Form alpha) = null alpha || any (guarded gs) alpha

instance Guarded (Alt' r a) where
  guarded gs (Alt _ sf) = guarded gs sf

instance Guarded (NTDef' x r a) where
  guarded gs (NTDef _ alts) = all (guarded gs) alts

-- Tool box for iteration

type Change = Writer Any

dirty :: Change ()
dirty = tell $ Any True

-- | Iterate until no change.

saturate :: (a -> Change a) -> a -> a
saturate f = loop
  where
  loop x = case runWriter $ f x of
    (y, Any True)  -> loop y
    (y, Any False) -> y

-- | Nullability.

class Nullable a where
  nullable :: a -> Bool
-}


-- | CYK-like parser (does not do optimal sharing if grammar not in 2NF).

parseWith :: Grammar -> String -> Err ()
parseWith grm inp = throwError "NYI: CYK parser"
