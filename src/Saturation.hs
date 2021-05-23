{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Saturation where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Monoid (Any(..))

import DebugPrint

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

-- | Iterate forever

iterateChange :: (a -> Change a) -> a -> [Change a]
iterateChange f a = iterate (f . fst . runWriter) (dirty >> return a)

-- * Printing

instance DebugPrint a => DebugPrint (Change a) where
  debugPrint w = case runWriter w of
    (a, Any b) -> unwords [ if b then "(dirty)" else "(clean)", debugPrint a ]
