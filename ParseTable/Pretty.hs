{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ParseTable.Pretty where

import Control.Arrow (first, second)

import Data.List (intercalate)
import qualified Data.List as List
import qualified Data.List.NonEmpty as List1
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Text.PrettyPrint.Boxes

import CFG
import DebugPrint
import ParseTable

instance {-# OVERLAPPABLE #-} (DebugPrint t) => DebugPrint (Input' t) where
  debugPrint ts = unwords $ map debugPrint ts

instance DebugPrint r => DebugPrint (NT' r) where
  debugPrint = debugPrint . ntNam

instance (DebugPrint r, DebugPrint t) => DebugPrint (Symbol' r t) where
  debugPrint (Term t)    = debugPrint t
  debugPrint (NonTerm x) = debugPrint x

instance (DebugPrint r, DebugPrint t) => DebugPrint (Stack' r t) where
  debugPrint s = unwords $ map debugPrint $ reverse s

instance (DebugPrint r, DebugPrint t) => DebugPrint (SRState' r t) where
  debugPrint (SRState s inp) = unwords [ debugPrint s, "\t.", debugPrint inp ]

instance (DebugPrint r) => DebugPrint (Rule' r t) where
  debugPrint (Rule x (Alt r alpha)) = debugPrint r

instance (DebugPrint r) => DebugPrint (SRAction' r t) where
  debugPrint Shift      = "shift"
  debugPrint (Reduce r) = unwords [ "reduce with rule", debugPrint r ]

instance (DebugPrint r) => DebugPrint (Action' r t) where
  debugPrint Nothing  = "halt"
  debugPrint (Just a) = debugPrint a

instance (DebugPrint r, DebugPrint t) => DebugPrint (TraceItem' r t) where
  debugPrint (TraceItem s a) = concat [ debugPrint s, "\t", debugPrint a ]

instance (DebugPrint r, DebugPrint t) => DebugPrint (Trace' r t) where
  debugPrint tr = unlines $ map debugPrint tr

instance DebugPrint IGotoActions where
  debugPrint gotos = unlines $ map ("\t" ++) $ map row $ IntMap.toList gotos
    where
    row (x, s) = unwords [ "NT", show x, "\tgoto state", show s ]

instance (DebugPrint r, DebugPrint t) => DebugPrint (ISRAction' r t) where
  debugPrint (ISRAction mshift rs) = intercalate ";" $ filter (not . null) $
    [ maybe "" (\ s -> unwords [ "shift to", show s ]) mshift
    , if null rs then ""
      else unwords [ "reduce with", intercalate " or " $ map debugPrint $ Set.toList rs ]
    ]

instance (Ord r, Ord t, DebugPrint r, DebugPrint t) => DebugPrint (ISRActions' r t) where
  debugPrint (ISRActions aeof tmap) = unlines $ map ("\t" ++) $
    (if aeof == mempty then id else (concat [ "eof", "\t", debugPrint aeof ] :)) $
      map (\(t,act) -> concat [ debugPrint t, "\t", debugPrint act ]) (Map.toList tmap)

instance (Ord r, Ord t, DebugPrint r, DebugPrint t) => DebugPrint (IPT' r t) where
  debugPrint (IPT sr goto) = unlines $ concat $ (`map` srgoto) $ \ (s, ls) ->
      [ unwords [ "State", show s ]
      , ""
      , ls
      ]
    where
    sr'    = map (second debugPrint) $ IntMap.toList sr
    goto'  = map (second debugPrint) $ IntMap.toList goto
    srgoto = IntMap.toList $ IntMap.fromListWith (\ s g -> unlines [s,g]) $ goto' ++ sr'
