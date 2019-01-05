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
import ParseTable

class DebugPrint a where
  debugPrint :: a -> String

instance DebugPrint IGotoActions where
  debugPrint gotos = unlines $ map ("\t" ++) $ map row $ IntMap.toList gotos
    where
    row (x, s) = unwords [ "NT", show x, "\tgoto state", show s ]

instance (Show r, Show t) => DebugPrint (Rule' r t) where
  debugPrint (Rule x (Alt r alpha)) = show r

instance (Show r, Show t) => DebugPrint (ISRAction' r t) where
  debugPrint (ISRAction mshift rs) = intercalate ";" $ filter (not . null) $
    [ maybe "" (\ s -> unwords [ "shift to", show s ]) mshift
    , if null rs then ""
      else unwords [ "reduce with", intercalate " or " $ map debugPrint $ Set.toList rs ]
    ]

instance (Ord r, Ord t, Show r, Show t) => DebugPrint (ISRActions' r t) where
  debugPrint (ISRActions aeof tmap) = unlines $ map ("\t" ++) $
    (if aeof == mempty then id else (concat [ "eof", "\t", debugPrint aeof ] :)) $
      map (\(t,act) -> concat [ show t, "\t", debugPrint act ]) (Map.toList tmap)

instance (Ord r, Ord t, Show r, Show t) => DebugPrint (IPT' r t) where
  debugPrint (IPT sr goto) = unlines $ concat $ (`map` srgoto) $ \ (s, ls) ->
      [ unwords [ "State", show s ]
      , ""
      , ls
      ]
    where
    sr'    = map (second debugPrint) $ IntMap.toList sr
    goto'  = map (second debugPrint) $ IntMap.toList goto
    srgoto = IntMap.toList $ IntMap.fromListWith (\ s g -> unlines [s,g]) $ goto' ++ sr'

-- instance (Ord r, Ord t, Show r, Show t) => DebugPrint (IPT' r t) where
--   debugPrint (IPT sr goto) = unlines $ concat
--     [ [ "Shift/reduce actions"
--       , "====================="
--       , ""
--       ]
--     , concat $ (`map` IntMap.toList sr) $ \ (s,act) ->
--         [ unwords [ "State", show s ]
--         , ""
--         , debugPrint act
--         ]
--     , [ "Goto actions"
--       , "============"
--       , ""
--       ]
--     , concat $ (`map` IntMap.toList goto) $ \ (s,act) ->
--         [ unwords [ "State", show s ]
--         , ""
--         , debugPrint act
--         ]
--     ]
