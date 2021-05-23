{-# LANGUAGE RankNTypes #-}

module Util where

import Control.Monad.State

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Lens.Micro
import Lens.Micro.Extras

use :: MonadState s m => Lens s s a a -> m a
use l = gets $ view l
  -- eta-expanded for GHC-9.0

modifying :: MonadState s m => Lens s s a a -> (a -> a) -> m ()
modifying l = modify . over l

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- -- | Silly workaround for lack of features in Data.Map.
-- --   Worst-case: 3 map operations (each logarithmic).

-- mapLookupInsert :: (Ord k) => k -> v -> Map k v -> (Maybe (k, v), Map k v)
-- mapLookupInsert k v m =
--   case Map.lookupIndex k m of
--     Nothing -> (Nothing, m')
--     Just i  -> (Just (Map.elemAt i m), m')
--   where
--   m' = Map.insert k v m
