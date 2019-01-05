{-# LANGUAGE LambdaCase #-}
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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe (listToMaybe)

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

import qualified LBNF.Abs as A
import LBNF.Print (Print, printTree)

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
  deriving (Eq, Show)

-- | A trace is a list of pairs of states and actions.

data TraceItem' r t = TraceItem { _trState :: SRState' t, _trAction :: Action' r t }

type Trace' r t = [TraceItem' r t]

-- | The next action is decided by a control function.

type Control' r t m = SRState' t -> MaybeT m (SRAction' r t)
-- type Control' r t m = SRState' t -> m (Action' r t)

-- | Run a shift-reduce parser given by control function on some input,
--   Returning a trace of states and actions.

runShiftReduceParser :: (Eq t, Monad m) => Control' r t m -> Input' t -> m (Trace' r t)
runShiftReduceParser f input = loop $ SRState [] input
  where
  loop st@(SRState stk ts0) = do
    act <- runMaybeT $ f st
    -- act <- f st
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
--   State 'Nothing' is the initial state.
--   Non-terminal 'Nothing' is the end of file.
--   For non-terminals, either a shift or a reduce action is returned.
--   For terminals, a goto action (next state) is returned.
--   If 'Nothing' is returned, the parser halts.

data ParseTable' r t s = ParseTable
  { _tabSR   :: Maybe s -> Maybe t -> Maybe (Either s (Rule' r t))
  , _tabGoto :: Maybe s -> NT      -> Maybe s
  }
makeLenses ''ParseTable'

-- | A LR control stack is a list of states.
--   An empty stack denotes the initial state.

type LRStack' s = [s]

-- | The LR control function modifies a control stack.
--   It interprets the parse table.

lr1Control :: ParseTable' r t s -> Control' r t (State (LRStack' s))
lr1Control (ParseTable tabSR tabGoto) (SRState stk input) = do
  -- Get control stack
  ss <- get
  -- Query table on maybe top state and maybe first input token.
  (MaybeT $ return $ tabSR (listToMaybe ss) (listToMaybe input)) >>= \case
    -- Shift action:
    Left s -> do
      -- Put new state on top of stack
      modify (s:)
      return Shift
    -- Reduce action:
    Right rule@(Rule x (Alt _ (Form alpha))) -> do
      -- Pop |alpha| many states
      let n = length alpha
      let (ss1, ss2) = splitAt n ss
      guard $ length ss1 /= n  -- internal error, halt!
      -- Execute the goto action
      s <- MaybeT $ return $ tabGoto (listToMaybe ss2) x
      put (s:ss2)
      return $ Reduce rule

-- lr1Control :: ParseTable' r t s -> Control' r t (State (LRStack' s))
-- lr1Control (ParseTable tabSR tabGoto) (SRState stk input) = do
--   ss <- get
--   let mts = List.uncons input
--   -- Query table on maybe top state and maybe first input token.
--   case tabSR (fst <$> List.uncons ss) (fst <$> mts) of
--     Nothing       -> return Nothing
--     -- Shift action:
--     Just (Left s) -> do
--       -- Put new state on top of stack
--       modify (s:)
--       return $ Just Shift
--     -- Reduce action:
--     Just (Right rule@(Rule x (Alt _ (Form alpha)))) -> do
--       -- Pop |alpha| many states
--       let n = length alpha
--       let (ss1, ss2) = splitAt n ss
--       if length ss1 /= n then return Nothing else do -- internal error, halt!
--       -- Execute the goto action
--       case tabGoto (fst <$> List.uncons ss2) x of
--         Nothing -> return Nothing
--         Just s -> do
--           put (s:ss2)
--           return $ Just $ Reduce rule


-- LR(1) parsetable generation.

-- | A parse item is a dotted rule X → α.β with a set of lookahead symbols.

data ParseItem' r t = ParseItem
  { _piRule   :: Rule' r t    -- ^ The rule this item comes from.
  , _piRest   :: [Symbol' t]  -- ^ The rest after the ".".
  , _piFollow :: SetMaybe' t  -- ^ The set of lookahead symbols.
  }

-- | A set of Maybe t is stored as a set of t plus a flag wether 'Nothing' is in the set.
data SetMaybe' t = SetMaybe { _smSet :: Set t, _smNothing :: Bool }
makeLenses ''ParseItem'
makeLenses ''SetMaybe'

-- | A parse state is a set of parse items.

type ParseState' r t = Set (ParseItem' r t)

-- | Completing a parse state.

--

-- complete :: ParseState' r t -> ParseState' r t
-- complete is
