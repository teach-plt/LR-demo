{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -Wunused-imports #-}

-- | Abstract syntax instance for grammars with single-character tokens.

module CharacterTokenGrammar where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Semigroup ((<>))

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)

import qualified LBNF.Abs as A
import LBNF.Print (printTree)

import CFG
import DebugPrint

-- | Grammar over single-character terminals with identifiers as rule names.

type Term     = Char
type NTName   = A.Ident
type RuleName = A.Ident
type Grammar  = Grammar' NTName RuleName Term
type NT       = NT' NTName
type NTDef    = NTDef' NTName RuleName Term
type Form     = Form' Term

-- | Intermediate rule format.
type IRule = (NT, RuleName, [A.Entry])

type Error = Either String

-- | Convert grammar to internal format; check for single-character terminals.
--   Also return start non-terminal if the grammar has any rules
checkGrammar :: A.Grammar -> Error (Maybe NT, Grammar)
checkGrammar (A.Rules rs) = (`runStateT` emptyGrammar) $ do
  irs <- mapM addNT rs
  mapM_ addRule irs
  -- The start symbol is the lhs of the first rule
  return $ listToMaybe $ map (\ (x, _, _) -> x) irs
  where
  -- Pass 1: collect non-terminals from lhss of rules.
  addNT :: A.Rule -> StateT Grammar Error IRule
  addNT (A.Prod r x es) = StateT $ \ grm@(Grammar n dict defs) -> do
    -- Check if we have seen NT x before.
    case Map.lookup x dict of
      -- Yes, use its number.
      Just i  -> return ((NT i x, r, es), grm)
      -- No, insert a new entry into the dictionary.
      Nothing -> return ((NT n x, r, es), Grammar (n+1) (Map.insert x n dict) defs)

  -- Pass 2: scope-check and convert rhss of rules.
  addRule :: IRule -> StateT Grammar Error ()
  addRule (NT i x, r, es) = StateT $ \ grm -> do
    alt <- Alt r . Form <$> do
     forM es $ \case
      -- Current limitation: Since we have no lexer, terminals are characters.
      A.Term [a] -> return $ Term a
      A.Term _   -> throwError "terminals must be single-character strings"
      -- Convert non-terminal names into de Bruijn indices (numbers).
      A.NT y -> case Map.lookup y $ view grmNTDict grm of
        Nothing -> throwError $ "undefined non-terminal " ++ printTree y
        Just j  -> return $ NonTerm $ NT j y
    return ((), over grmNTDefs (IntMap.insertWith (<>) i (NTDef x [alt])) grm)

-- | Turn grammar back to original format.

reifyGrammar :: Grammar -> A.Grammar
reifyGrammar grm@(Grammar _ dict defs) =
  A.Rules . (`concatMap` IntMap.toList defs) $ \ (i, NTDef x alts) ->
    (`map` alts) $ \ (Alt r (Form alpha)) ->
      A.Prod r x . (`map` alpha) $ \case
        Term a    -> A.Term [a]
        NonTerm j -> A.NT $ ntToIdent grm j

ntToIdent :: Grammar -> NT -> NTName
ntToIdent grm (NT i x) = x
-- -- Lookup in Grammar no longer needed as NT's carry their name.
-- ntToIdent grm (NT i x) =
--   view ntName $
--     IntMap.findWithDefault (error "printGrammar: impossible") i $
--       view grmNTDefs grm

-- * Printing

instance DebugPrint (A.Ident) where
  debugPrint (A.Ident s) = s

instance DebugPrint Term where
  debugPrint = show
  -- debugPrint = (:[])
