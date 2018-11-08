{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}

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
import LBNF.ErrM

import CFG

-- | Grammar over single-character terminals with identifiers as rule names.

type NTName   = A.Ident
type RuleName = A.Ident
type Grammar  = Grammar' NTName RuleName Char
type NTDef    = NTDef' NTName RuleName Char
type Form     = Form' Char

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
run s = case pGrammar (myLexer s) of
  Bad err  -> do
    putStrLn "Syntax error in grammar file"
    putStrLn err
    exitFailure
  Ok tree -> case checkGrammar tree of
    Left err -> do
      putStrLn "Error in grammar"
      putStrLn err
      exitFailure
    Right grm -> do
      putStrLn "Parsing stdin with the following grammar:"
      putStrLn $ printTree $ reifyGrammar grm
      runM $ checkGuardedness grm
      stdin <- getContents
      case parseWith grm stdin of
        Left err -> do
          putStrLn "Parse failed"
          putStrLn err
          exitFailure
        Right _ -> do
          putStrLn "Parse successful!"

type M = Either String

runM :: M a -> IO a
runM = \case
  Right a -> return a
  Left err -> do
    putStrLn $ "Error: " ++ err
    exitFailure

-- | Intermediate rule format.
type IRule = (NT, NTName, RuleName, [A.Entry])

-- | Convert grammar to internal format; check for single-character terminals.

checkGrammar :: A.Grammar -> M Grammar
checkGrammar (A.Rules rs) = (`execStateT` emptyGrammar) $ do
  mapM_ addRule =<< mapM addNT rs
  where
  addNT :: A.Rule -> StateT Grammar M IRule
  addNT (A.Prod r x es) = StateT $ \ grm@(Grammar n dict defs) -> do
    -- Check if we have seen NT x before.
    case Map.lookup x dict of
      -- Yes, use its number.
      Just i  -> return ((i, x, r, es), grm)
      -- No, insert a new entry into the dictionary.
      Nothing -> return ((n, x, r, es), Grammar (n+1) (Map.insert x n dict) defs)

  addRule :: IRule -> StateT Grammar M ()
  addRule (i, x, r, es) = StateT $ \ grm -> do
    alt <- Alt r . Form <$> do
     forM es $ \case
      A.Term [a] -> return $ Term a
      A.Term _   -> throwError "terminals must be single-character strings"
      A.NT y -> case Map.lookup y $ view grmNTDict grm of
        Nothing -> throwError $ "undefined non-terminal " ++ printTree y
        Just j  -> return $ NT j
    return ((), over grmNTDefs (IntMap.insertWith (<>) i (NTDef x [alt])) grm)--{ grmNTDefs = IntMap.insertWith (++) i [alt] $ grmNTDefs grm })

-- | Turn grammar back to original format.

reifyGrammar :: Grammar -> A.Grammar
reifyGrammar grm@(Grammar _ dict defs) =
  A.Rules . (`concatMap` IntMap.toList defs) $ \ (i, NTDef x alts) ->
    (`map` alts) $ \ (Alt r (Form alpha)) ->
      A.Prod r x . (`map` alpha) $ \case
        Term a -> A.Term [a]
        NT j   -> A.NT $ ntToIdent grm j
  where
  rdict = IntMap.fromList $ map swap $ Map.toList dict

ntToIdent :: Grammar -> NT -> NTName
ntToIdent grm i = view ntName $
  IntMap.findWithDefault (error "printGrammar: impossible") i $ view grmNTDefs grm

checkGuardedness :: Grammar -> M ()
checkGuardedness grm@(Grammar n dict defs) = do
  let gs = computeGuardedness grm
  unless (all getGuarded gs) $ do
    let is = mapMaybe (\ (i, g) -> if getGuarded g then Nothing else Just i) $ IntMap.toList gs
    let us = map (printTree . ntToIdent grm) is
    throwError $ "ungarded non-terminals in grammar: " ++ unwords us

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

parseWith :: Grammar -> String -> M ()
parseWith grm inp = throwError "NYI: CYK parser"
