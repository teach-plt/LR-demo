-- Generate LR parser for:
-- S ::= S ">" ;

module BugLRec where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import DebugPrint
import Saturation
import SetMaybe

import CFG
import CharacterTokenGrammar
import ParseTable
import ParseTable.Pretty

-- S ::= S ">" ;

s    = NT 0 "S"
rule = NTDef "S" [Alt "S>" $ Form [ NonTerm s , Term '>' ]]
grm  = Grammar 1 (Map.singleton "S" 0) (IntMap.singleton 0 rule)
egrm = makeEGrammar grm s
state0 = ptState0 egrm
state0step = completeStep egrm state0
evol n = take n $ iterateChange (completeStep egrm) state0
pevol n = putStr $ unlines $ map debugPrint $ evol n

set = SetMaybe (Set.fromList ">") True

{-
*Main> evol 3
[ WriterT (Identity
   ( ParseState {theParseState = fromList [(ParseItem {_piRule = Rule (NT {ntNum = 0, ntNam = "S"}) (Alt "S>" (Form {theForm = [NonTerm (NT {ntNum = 0, ntNam = "S"}),Term '>']})), _piRest = [NonTerm (NT {ntNum = 0, ntNam = "S"}),Term '>']}
   , SetMaybe {_smSet = fromList "", _smNothing = True})]}
  , Any {getAny = True}))
, WriterT (Identity
    ( ParseState {theParseState = fromList [(ParseItem {_piRule = Rule (NT {ntNum = 0, ntNam = "S"}) (Alt "S>" (Form {theForm = [NonTerm (NT {ntNum = 0, ntNam = "S"}),Term '>']})), _piRest = [NonTerm (NT {ntNum = 0, ntNam = "S"}),Term '>']}
    , SetMaybe {_smSet = fromList ">", _smNothing = True})]}
  , Any {getAny = True}))
, WriterT (Identity
    (ParseState {theParseState = fromList [(ParseItem {_piRule = Rule (NT {ntNum = 0, ntNam = "S"}) (Alt "S>" (Form {theForm = [NonTerm (NT {ntNum = 0, ntNam = "S"}),Term '>']})), _piRest = [NonTerm (NT {ntNum = 0, ntNam = "S"}),Term '>']}
    , SetMaybe {_smSet = fromList ">", _smNothing = True})]}
  , Any {getAny = True}))
]
-}
