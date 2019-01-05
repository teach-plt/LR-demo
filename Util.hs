{-# LANGUAGE RankNTypes #-}

module Util where

import Control.Monad.State
import Lens.Micro
import Lens.Micro.Extras

use :: MonadState s m => Lens s s a a -> m a
use = gets . view

modifying :: MonadState s m => Lens s s a a -> (a -> a) -> m ()
modifying l = modify . over l
