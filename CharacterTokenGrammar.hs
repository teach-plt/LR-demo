{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- {-# OPTIONS_GHC -Wunused-imports #-}

-- | Abstract syntax instance for grammars with single-character tokens.

module CharacterTokenGrammar where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)

import qualified LBNF.Abs as A
import LBNF.Print (printTree)

import CFG

-- | Grammar over single-character terminals with identifiers as rule names.

type NTName   = A.Ident
type RuleName = A.Ident
type Grammar  = Grammar' NTName RuleName Char
type NTDef    = NTDef' NTName RuleName Char
type Form     = Form' Char

-- | Intermediate rule format.
type IRule = (NT, NTName, RuleName, [A.Entry])

type Error = Either String

-- | Convert grammar to internal format; check for single-character terminals.

checkGrammar :: A.Grammar -> Error Grammar
checkGrammar (A.Rules rs) = (`execStateT` emptyGrammar) $ do
  mapM_ addRule =<< mapM addNT rs
  where
  -- Pass 1: collect non-terminals from lhss of rules.
  addNT :: A.Rule -> StateT Grammar Error IRule
  addNT (A.Prod r x es) = StateT $ \ grm@(Grammar n dict defs) -> do
    -- Check if we have seen NT x before.
    case Map.lookup x dict of
      -- Yes, use its number.
      Just i  -> return ((i, x, r, es), grm)
      -- No, insert a new entry into the dictionary.
      Nothing -> return ((n, x, r, es), Grammar (n+1) (Map.insert x n dict) defs)

  -- Pass 2: scope-check and convert rhss of rules.
  addRule :: IRule -> StateT Grammar Error ()
  addRule (i, x, r, es) = StateT $ \ grm -> do
    alt <- Alt r . Form <$> do
     forM es $ \case
      -- Current limitation: Since we have no lexer, terminals are characters.
      A.Term [a] -> return $ Term a
      A.Term _   -> throwError "terminals must be single-character strings"
      -- Convert non-terminal names into de Bruijn indices (numbers).
      A.NT y -> case Map.lookup y $ view grmNTDict grm of
        Nothing -> throwError $ "undefined non-terminal " ++ printTree y
        Just j  -> return $ NT j
    return ((), over grmNTDefs (IntMap.insertWith (<>) i (NTDef x [alt])) grm)

-- | Turn grammar back to original format.

reifyGrammar :: Grammar -> A.Grammar
reifyGrammar grm@(Grammar _ dict defs) =
  A.Rules . (`concatMap` IntMap.toList defs) $ \ (i, NTDef x alts) ->
    (`map` alts) $ \ (Alt r (Form alpha)) ->
      A.Prod r x . (`map` alpha) $ \case
        Term a -> A.Term [a]
        NT j   -> A.NT $ ntToIdent grm j

ntToIdent :: Grammar -> NT -> NTName
ntToIdent grm i = view ntName $
  IntMap.findWithDefault (error "printGrammar: impossible") i $ view grmNTDefs grm