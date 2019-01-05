{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | LR-parser.

module ParseTable where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Maybe

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.List.NonEmpty as List1
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (maybeToList, listToMaybe)

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

import qualified LBNF.Abs as A
import LBNF.Print (Print, printTree)

import SetMaybe (SetMaybe)
import qualified SetMaybe

import Saturation
import CFG
import CharacterTokenGrammar

-- Shift-reduce parser.

-- | A stack is a sentential form (reversed).

type Stack' t = [Symbol' t]
type Input' t = [t]

-- | The state of a shift-reduce parser consists of a stack and some input.

data SRState' t = SRState { _srStack :: Stack' t, _srInput :: Input' t }
makeLenses ''SRState'

-- | An action of a shift-reduce parser.

data SRAction' r t
  = Shift               -- ^ Shift next token onto stack.
  | Reduce (Rule' r t)  -- ^ Reduce with given rule.

type Action' r t = Maybe (SRAction' r t)  -- ^ Nothing means halt.

data Rule' r t = Rule NT (Alt' r t)
  deriving (Eq, Ord, Show)

-- | A trace is a list of pairs of states and actions.

data TraceItem' r t = TraceItem { _trState :: SRState' t, _trAction :: Action' r t }

type Trace' r t = [TraceItem' r t]

-- | The next action is decided by a control function.

type Control' r t m = SRState' t -> MaybeT m (SRAction' r t)

-- | Run a shift-reduce parser given by control function on some input,
--   Returning a trace of states and actions.

runShiftReduceParser :: (Eq t, Monad m) => Control' r t m -> Input' t -> m (Trace' r t)
runShiftReduceParser f input = loop $ SRState [] input
  where
  loop st@(SRState stk ts0) = do
    act <- runMaybeT $ f st
    (TraceItem st act :) <$> do
      case (act, ts0) of
        (Just Shift                   , t:ts) -> loop $ SRState (Term t : stk) ts
        (Just (Reduce (Rule x (Alt r alpha))), _)
          | Just stk' <- matchStack stk alpha -> loop $ SRState (NT x : stk') ts0
        _ -> halt

  matchStack stk (Form alpha) = List.stripPrefix (reverse alpha) stk
  halt = return []


-- | A parse table maps pairs of states and symbols to actions.
--
--   Non-terminal 'Nothing' is the end of file.
--   For non-terminals, either a shift or a reduce action is returned.
--   For terminals, a goto action (next state) is returned.
--   If 'Nothing' is returned, the parser halts.

data ParseTable' r t s = ParseTable
  { _tabSR   :: s -> Maybe t -> Maybe (Either s (Rule' r t))
  , _tabGoto :: s -> NT      -> Maybe s
  , _tabInit :: s
  }
makeLenses ''ParseTable'

-- | A LR control stack is a non-empty list of states.
--   The bottom element is the initial state.

type LRStack' s = List1.NonEmpty s

-- | The LR control function modifies a control stack.
--   It interprets the parse table.

lr1Control :: ParseTable' r t s -> Control' r t (State (LRStack' s))
lr1Control (ParseTable tabSR tabGoto _) (SRState stk input) = do
  -- Get control stack
  ss <- get
  -- Query table on maybe top state and maybe first input token.
  (MaybeT $ return $ tabSR (List1.head ss) (listToMaybe input)) >>= \case
    -- Shift action:
    Left s -> do
      -- Put new state on top of stack
      modify (List1.cons s)
      return Shift
    -- Reduce action:
    Right rule@(Rule x (Alt _ (Form alpha))) -> do
      -- Pop |alpha| many states
      let n = length alpha
      let (ss1, rest) = List1.splitAt n ss
      guard $ length ss1 /= n  -- internal error, halt!
      -- Rest should be non-empty, otherwise internal error.
      ss2 <- MaybeT $ return $ List1.nonEmpty rest
      -- Execute the goto action
      s <- MaybeT $ return $ tabGoto (List1.head ss2) x
      put (List1.cons s ss2)
      return $ Reduce rule

-- | Run the LR(1) parser with the given parsetable.

runLR1Parser :: (Eq t) => ParseTable' r t s -> Input' t -> Trace' r t
runLR1Parser pt@(ParseTable _ _ s0) input =
  runShiftReduceParser control input `evalState` (s0 List1.:| [])
  where
  control = lr1Control pt

-- LR(1) parsetable generation.

-- | A parse item is a dotted rule X → α.β.

data ParseItem' r t = ParseItem
  { _piRule   :: Rule' r t    -- ^ The rule this item comes from.
  , _piRest   :: [Symbol' t]  -- ^ The rest after the ".".
  }
  deriving (Eq, Ord, Show)
makeLenses ''ParseItem'

type Lookahead t = SetMaybe t  -- ^ The set of lookahead symbols.

-- | A parse state is a map of parse items to lookahead lists.

type ParseState' r t = Map (ParseItem' r t) (Lookahead t)

-- | Completing a parse state.
--
--   For each (X → α.Yβ, ts), add (Y → .γ, FIRST(β)∘ts)).
--   This might add a whole new item or just extend the token list.

complete :: forall x r t. (Ord r, Ord t)
  => EGrammar' x r t
  -> ParseState' r t
  -> ParseState' r t
complete (EGrammar (Grammar _ _ ntDefs) _ fs) = saturate step
  where
  step :: ParseState' r t -> Change (ParseState' r t)
  step is = mapM_ add
      [ (ParseItem (Rule y alt) gamma, la')
      | (ParseItem _ (NT y : beta), la) <- Map.toList is
      , NTDef _ alts                    <- maybeToList $ IntMap.lookup y ntDefs
      , alt@(Alt _ (Form gamma))        <- alts
      , let la' = getFirst $ concatFirst (firstSet fs $ Form beta) $ First la
      ]
      `execStateT` is
    where
    -- Add a parse item candidate.
    add :: (ParseItem' r t, Lookahead t) -> StateT (ParseState' r t) Change ()
    add (k, new) = do
      st <- get
      let (mv, st') = Map.insertLookupWithKey (\ _ -> SetMaybe.union) k new st
      put st'
      -- Detect change:
      case mv of
        -- Item is new?
        Nothing -> lift dirty
        -- Item is old, maybe lookahead is new?
        Just old -> unless (SetMaybe.isSubsetOf old new) $ lift dirty

-- | Goto action for a parse state.

-- data Successors' r t = Successors
--   { _sucEof :: Maybe

--successors :: ParseState' r t -> (Map (Term t) (ParseState' r t), IntMap (ParseState' r t))
successors :: (Ord r, Ord t) => EGrammar' x r t -> ParseState' r t -> Map (Symbol' t) (ParseState' r t)
successors grm is = complete grm <$> Map.fromListWith (Map.unionWith SetMaybe.union)
  [ (sy, Map.singleton (ParseItem r alpha) la)
  | (ParseItem r (sy : alpha), la) <- Map.toList is
  ]

-- ParseState dictionary

type PState = Int

initPState = 0

type PSDict' r t = Map (ParseState' r t) PState

-- Internal parse table

data IPT' r t = IPT
  { _iptSR   :: IntMap (Map (Maybe t) (Maybe (Either PState (Rule' r t))))
  , _iptGoto :: IntMap (IntMap (Maybe PState))
  }

-- Parse table generator state

data PTGenState' r t = PTGenState
  { _stNext   :: Int            -- ^ Next unused state number.
  , _stPSDict :: PSDict' r t    -- ^ Translation from states to state numbers.
  , _stIPT    :: IPT' r t       -- ^ Internal parse table.
  }

ptGen :: forall x r t. (Ord r, Ord t) => EGrammar' x r t -> NT -> IPT' r t
ptGen grm@(EGrammar (Grammar _ _ ntDefs) start fs) = undefined
  where
  laEOF  = SetMaybe.singleton Nothing
  alts0  = maybe [] (view ntDef) $ IntMap.lookup start ntDefs
  items0 = map (\ alt@(Alt r (Form alpha)) -> (ParseItem (Rule start alt) alpha, laEOF)) alts0
  state0 = complete grm (Map.fromList items0)
