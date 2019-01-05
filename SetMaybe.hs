{-# LANGUAGE TemplateHaskell #-}

-- | Sets of @Maybe a@ values.

module SetMaybe where

import Data.Set (Set)
import qualified Data.Set as Set

-- uses microlens-platform
import Lens.Micro.TH (makeLenses)

-- | A set of @Maybe t@ is stored as a set of @t@
--   plus a flag wether 'Nothing' is in the set.

data SetMaybe t = SetMaybe { _smSet :: Set t, _smNothing :: Bool }

makeLenses ''SetMaybe

empty :: SetMaybe t
empty  = SetMaybe Set.empty False

setOfNothing :: SetMaybe t
setOfNothing = SetMaybe Set.empty True

singleton :: Maybe t -> SetMaybe t
singleton Nothing = setOfNothing
singleton (Just k) = SetMaybe (Set.singleton k) False

-- | Union.

union :: Ord t => SetMaybe t -> SetMaybe t -> SetMaybe t
union (SetMaybe s b) (SetMaybe s' b') = SetMaybe (Set.union s s') (b || b')

-- | Query subset.

isSubsetOf :: Ord t => SetMaybe t -> SetMaybe t -> Bool
isSubsetOf (SetMaybe s b) (SetMaybe s' b') = (b' || not b) && Set.isSubsetOf s s'

-- | Query membership.

member :: Ord t => Maybe t -> SetMaybe t -> Bool
member Nothing  (SetMaybe _  b) = b
member (Just k) (SetMaybe ks _) = Set.member k ks
