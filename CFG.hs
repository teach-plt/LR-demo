{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Context-free grammars: syntax and grammar folds.

module CFG where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Foldable as Fold
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

import Saturation

-- | A grammar over non-terminal names x, rulenames r and an alphabet t
--   consists of definitions of the nonterminals, represented as Ints.

data Grammar' x r t = Grammar
  { _grmNumNT  :: Int                   -- ^ Number of non-terminals.
  , _grmNTDict :: Map x NT              -- ^ Names-to-number map for non-terminals.
  , _grmNTDefs :: IntMap (NTDef' x r t) -- ^ Definitions of non-terminals.
  }

emptyGrammar :: Grammar' x r t
emptyGrammar = Grammar 0 Map.empty IntMap.empty

-- | A nonterminal is defined by a list of alternatives.

data NTDef' x r t = NTDef { _ntName :: x, _ntDef :: [Alt' r t] }

-- | Each alternative is a rule name plus a sentential form.

data Alt' r t = Alt r (Form' t)

-- | A sentential form is a string of symbols.

newtype Form' t = Form [Symbol' t]
  deriving (Eq, Show)

-- | A symbol is a terminal or a non-terminal.

data Symbol' t
  = Term t
  | NT NT
  deriving (Eq, Show)

-- | Non-terminals are natural numbers.
type NT = Int

-- Lenses
makeLenses ''Grammar'
makeLenses ''NTDef'

-- | Disregarding 'NTName', we can join non-terminal definitions.

instance (Show x, Eq x) => Semigroup (NTDef' x r t) where
  NTDef x alts <> NTDef x' alts'
    | x == x'   = NTDef x $ alts ++ alts'
    | otherwise = error $ unwords $
       [ "non-terminal names do not match:" ] ++ map show [x, x']

-- * Generic grammar folds.

-- The class-based approach did not go well with Haskell's
-- instance inference.
--
-- class GrmAlg t a where
--   gaTerminal :: t -> a        -- ^ Single terminal.
--   gaZero     :: a             -- ^ Empty language.
--   gaPlus     :: a -> a -> a   -- ^ Language union.
--   gaEps      :: a             -- ^ Language of the empty word.
--   gaConcat   :: a -> a -> a   -- ^ Language concatenation.

-- class GrmFold t a b where
--   grmFold :: GrmAlg t a => (NT -> a) -> b -> a

-- | A grammar algebra provides an implementation for
--   the operations constituting CFGs.

data GrmAlg r t a = GrmAlg
  { gaTerminal :: t -> a        -- ^ Single terminal.
  , gaZero     :: a             -- ^ Empty language.
  , gaPlus     :: a -> a -> a   -- ^ Language union.
  , gaEps      :: a             -- ^ Language of the empty word.
  , gaConcat   :: a -> a -> a   -- ^ Language concatenation.
  , gaLabel    :: r -> a -> a   -- ^ Labelled language.
  }

-- | @n@-ary concatenation, with a special case for empty concatenation.
gaProduct :: GrmAlg r t a -> [a] -> a
gaProduct ga [] = gaEps ga
gaProduct ga as = foldl1 (gaConcat ga) as

-- | @n@-ary alternative, with a special case for empty language.
gaSum :: GrmAlg r t a -> [a] -> a
gaSum ga [] = gaZero ga
gaSum ga as = foldl1 (gaPlus ga) as

-- | Generic fold over a grammar.

class GrmFold r t a b where
  grmFold :: GrmAlg r t a -> (NT -> a) -> b -> a

instance GrmFold r t a NT where
  grmFold ga env i = env i

instance GrmFold r t a (Symbol' t) where
  grmFold ga env = \case
    Term t -> gaTerminal ga t
    NT i   -> env i

instance GrmFold r t a (Form' t) where
  grmFold ga env (Form alpha) = gaProduct ga $ map (grmFold ga env) alpha

instance GrmFold r t a (Alt' r t) where
  grmFold ga env (Alt r alpha) = gaLabel ga r (grmFold ga env alpha)

instance GrmFold r t a (NTDef' x r t) where
  grmFold ga env (NTDef _x alts) = gaSum ga $ map (grmFold ga env) alts


-- | Computing properties of non-terminals by saturation.
--   The iteration is needed to handle the recursion inherent in CFGs.
--   Requires a bounded lattice @a@.

grmIterate :: forall r t a x . (Eq a, Ord a)
  => GrmAlg r t a   -- ^ Grammar algebra.
  -> Grammar' x r t -- ^ Grammar.
  -> a              -- ^ Default/start value.
  -> a              -- ^ Best value.
  -> IntMap a       -- ^ Final value for each non-terminal.
grmIterate ga grm@(Grammar n dict defs) bot top
  = IntMap.map fst
  $ saturate (\ gs -> IntMap.traverseWithKey (step gs) gs)
  $ IntMap.map (bot,) defs
  where
  step :: IntMap (a, NTDef' x r t)
       -> NT
       -> (a, NTDef' x r t)
       -> Change (a, NTDef' x r t)
  step gs i d@(a, def)
    | a /= top, let a' = grmFold ga env def, a' > a = do
      dirty  -- change!
      return (a', def)
    | otherwise = return d  -- no change
    where
    env j = fst $ IntMap.findWithDefault (error "grmIterate") j gs

-- * Guardedness.

newtype Guarded = Guarded { getGuarded :: Bool }
  deriving (Eq, Ord, Show, Bounded) -- False < True

guardedAlg :: GrmAlg r t Guarded
guardedAlg = GrmAlg
  { gaTerminal = const maxBound  -- Yes. A terminal is guarded.
  , gaZero     = maxBound  -- Yes.  (A bit arbitrary, but consistent with gaPlus.)
  , gaPlus     = min       -- All alternatives need to be guarded.
  , gaEps      = maxBound  -- Empty language is guarded!  (Outlier!)
  , gaConcat   = max       -- One factor needs to be guarded.
  , gaLabel    = const id  -- Labels do not change the game.
  }

computeGuardedness :: Grammar' x r t -> IntMap Guarded
computeGuardedness grm = grmIterate guardedAlg grm minBound maxBound

-- * Nullability.

newtype Nullable = Nullable { getNullable :: Bool }
  deriving (Eq, Ord, Show, Bounded) -- False < True

nullableAlg :: GrmAlg r t Nullable
nullableAlg = GrmAlg
  { gaTerminal = const minBound  -- No. A terminal is not nullable.
  , gaZero     = minBound  -- No.
  , gaPlus     = max       -- One alternative suffices.
  , gaEps      = maxBound  -- Yes. Empty language is exactly nullable.
  , gaConcat   = min       -- All factor must be nullable.
  , gaLabel    = const id  -- Labels do not change the game.
  }

computeNullable :: Grammar' x r t -> IntMap Nullable
computeNullable grm = grmIterate nullableAlg grm minBound maxBound
