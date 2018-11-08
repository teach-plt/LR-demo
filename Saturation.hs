module Saturation where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Monoid (Any(..))

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
