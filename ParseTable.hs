{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

-- | LR-parser.

module ParseTable where

import Control.Arrow (first, second)
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

import Data.Function (on)
import Data.Maybe (catMaybes, maybeToList, listToMaybe, fromMaybe)
import Data.Either (partitionEithers)

-- uses microlens-platform
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)

import qualified LBNF.Abs as A
import LBNF.Print (Print, printTree)

import SetMaybe (SetMaybe(SetMaybe))
import qualified SetMaybe

import Util
import Saturation
import CFG
import CharacterTokenGrammar

-- Shift-reduce parser.

-- | A stack is a sentential form (reversed).

type Stack' t = [Symbol' t]
type Input' t = [t]

-- | The state of a shift-reduce parser consists of a stack and some input.

data SRState' t = SRState { _srStack :: Stack' t, _srInput :: Input' t }
  deriving (Show)
makeLenses ''SRState'

-- | An action of a shift-reduce parser.

data SRAction' r t
  = Shift               -- ^ Shift next token onto stack.
  | Reduce (Rule' r t)  -- ^ Reduce with given rule.
  deriving (Show)

type Action' r t = Maybe (SRAction' r t)  -- ^ Nothing means halt.

data Rule' r t = Rule NT (Alt' r t)
  deriving (Eq, Ord, Show)

-- | A trace is a list of pairs of states and actions.

data TraceItem' r t = TraceItem { _trState :: SRState' t, _trAction :: Action' r t }
  deriving (Show)

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
        (Nothing                      , _   ) -> halt
        (Just Shift                   , t:ts) -> loop $ SRState (Term t : stk) ts
        (Just (Reduce (Rule x (Alt r alpha))), _)
          | Just stk' <- matchStack stk alpha -> loop $ SRState (NT x : stk') ts0
        _ -> error "runShiftReduceParser: reduce failed"

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
      -- Rest should be non-empty, otherwise internal error.
      let err = error $ "lr1Control: control stack to short to reduce"
      let ss2 = fromMaybe err $ List1.nonEmpty rest
      -- Execute the goto action (if present)
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

newtype ParseState' r t = ParseState { theParseState :: Map (ParseItem' r t) (Lookahead t) }
  deriving (Eq, Ord, Show)

-- fullyEqual :: (Eq r, Eq t) => ParseState' r t -> ParseState' r t -> Bool
-- fullyEqual (ParseState is) (ParseState is') = is == is'

-- -- | LALR: ignore the lookahead: fuse states with same items.

-- instance (Eq r, Eq t) => Eq (ParseState' r t) where
--   (==) = (==) `on` (Map.keysSet . theParseState)

-- instance (Ord r, Ord t) => Ord (ParseState' r t) where
--   compare = compare `on` (Map.keysSet . theParseState)

instance (Ord r, Ord t) => Semigroup (ParseState' r t) where
  ParseState is <> ParseState is' = ParseState $ Map.unionWith SetMaybe.union is is'

instance (Ord r, Ord t) => Monoid (ParseState' r t) where
  mempty = ParseState $ Map.empty
  mappend = (<>)

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
  step (ParseState is) = mapM_ add
      [ (ParseItem (Rule y alt) gamma, la')
      | (ParseItem _ (NT y : beta), la) <- Map.toList is
      , NTDef _ alts                    <- maybeToList $ IntMap.lookup y ntDefs
      , alt@(Alt _ (Form gamma))        <- alts
      , let la' = getFirst $ concatFirst (firstSet fs $ Form beta) $ First la
      ]
      `execStateT` ParseState is
    where
    -- Add a parse item candidate.
    add :: (ParseItem' r t, Lookahead t) -> StateT (ParseState' r t) Change ()
    add (k, new) = do
      ParseState st <- get
      let (mv, st') = Map.insertLookupWithKey (\ _ -> SetMaybe.union) k new st
      put $ ParseState st'
      -- Detect change:
      case mv of
        -- Item is new?
        Nothing -> lift dirty
        -- Item is old, maybe lookahead is new?
        Just old -> unless (SetMaybe.isSubsetOf old new) $ lift dirty

-- | Goto action for a parse state.

-- successors :: ParseState' r t -> (Map (Term t) (ParseState' r t), IntMap (ParseState' r t))
successors :: (Ord r, Ord t) => EGrammar' x r t -> ParseState' r t -> Map (Symbol' t) (ParseState' r t)
successors grm (ParseState is) = complete grm <$> Map.fromListWith (<>)
  [ (sy, ParseState $ Map.singleton (ParseItem r alpha) la)
  | (ParseItem r (sy : alpha), la) <- Map.toList is
  ]

-- ParseState dictionary

type PState = Int

initPState = 0

-- | LALR: LR0 automaton decorated with lookahead.
--   The @LR0State@ is the @keysSet@ of a @ParseState@.

type LR0State' r t = Set (ParseItem' r t)

lr0state :: ParseState' r t -> LR0State' r t
lr0state (ParseState is) = Map.keysSet is

-- The dictionary maps LR0 states to state numbers and their best decoration.
type PSDict' r t = Map (LR0State' r t) (PState, ParseState' r t)

-- Internal parse table

data IPT' r t = IPT
  { _iptSR   :: IntMap (ISRActions' r t)  -- ^ Map from states to shift-reduce actions.
  , _iptGoto :: IntMap IGotoActions       -- ^ Map from states to goto actions.
  }
  deriving (Show)

-- | Goto actions of a state.
--   Mapping non-terminals to successor states.

type IGotoActions = IntMap PState

-- | Shift-reduce actions of a state.

data ISRActions' r t = ISRActions
  { _iactEof  :: ISRAction' r t
  , _iactTerm :: Map t (ISRAction' r t)
  }
  deriving (Eq, Ord, Show)

instance (Ord r, Ord t) => Semigroup (ISRActions' r t) where
  ISRActions aeof atok <> ISRActions aeof' atok' =
    ISRActions (aeof <> aeof') (Map.unionWith (<>) atok atok')

instance (Ord r, Ord t) => Monoid (ISRActions' r t) where
  mempty = ISRActions mempty Map.empty
  mappend = (<>)

shiftActions :: (Ord r, Ord t) => Map t (ISRAction' r t) -> ISRActions' r t
shiftActions = ISRActions mempty

-- | Entry of a parse table cell: shift and/or reduce action(s).

data ISRAction'  r t = ISRAction
  { _iactShift  :: Maybe PState     -- ^ Possibly a shift action.
  , _iactReduce :: Set (Rule' r t)  -- ^ Possibly several reduce actions.
  }
  deriving (Eq, Ord, Show)

instance (Ord r, Ord t) => Semigroup (ISRAction' r t) where
  -- ISRAction Just{} _ <> ISRAction Just{} _ = error $ "impossible: union of shift actions"
  ISRAction ms1   r1 <> ISRAction ms2   r2 = ISRAction ms r
    where
    ms = listToMaybe $ maybeToList ms1 ++ maybeToList ms2
    r  = Set.union r1 r2

instance (Ord r, Ord t) => Monoid (ISRAction' r t) where
  mempty = emptyAction
  mappend = (<>)

emptyAction :: ISRAction' r t
emptyAction = ISRAction Nothing Set.empty

shiftAction :: PState -> ISRAction' r t
shiftAction s = ISRAction (Just s) Set.empty

reduceAction :: Rule' r t -> ISRAction' r t
reduceAction rule = ISRAction Nothing $ Set.singleton rule

-- | Compute the reduce actions for a parse state.

reductions :: (Ord r, Ord t) => ParseState' r t -> ISRActions' r t
reductions (ParseState is) = mconcat
    [ ISRActions (if eof then ra else emptyAction) (Map.fromSet (const ra) ts)
    | (ParseItem r [], SetMaybe ts eof) <- Map.toList is
    , let ra = reduceAction r
    ]

-- Parse table generator state

data PTGenState' r t = PTGenState
  { _stNext   :: Int            -- ^ Next unused state number.
  , _stPSDict :: PSDict' r t    -- ^ Translation from states to state numbers.
  , _stIPT    :: IPT' r t       -- ^ Internal parse table.
  }
makeLenses ''ISRAction'
makeLenses ''ISRActions'
makeLenses ''IPT'
makeLenses ''PTGenState'

ptGen :: forall x r t. (Ord r, Ord t) => EGrammar' x r t -> IPT' r t
ptGen grm@(EGrammar (Grammar _ _ ntDefs) start fs) =
  view stIPT $ loop [state0] `execState` stInit
  where
  stInit :: PTGenState' r t
  stInit = PTGenState 1 (Map.singleton (lr0state state0) (0, state0)) $
             IPT IntMap.empty IntMap.empty
             -- IPT (IntMap.singleton 0 $ reductions state0)
             --     (IntMap.singleton 0 $ IntMap.empty)  -- initially no goto actions

  -- The first state contains the productions for the start non-terminal.
  state0 :: ParseState' r t
  state0 = complete grm $ ParseState $ Map.fromList items0
    where
    laEOF  = SetMaybe.singleton Nothing
    alts0  = maybe [] (view ntDef) $ IntMap.lookup start ntDefs
    items0 = flip map alts0 $ \ alt@(Alt r (Form alpha)) ->
      (ParseItem (Rule start alt) alpha, laEOF)

  -- Work off worklist of registered by not processed parse states.
  loop :: [ParseState' r t] -> State (PTGenState' r t) ()
  loop [] = return ()
  loop (is : worklist) = do
    let k = lr0state is  -- the LR0State of is
    (Map.lookup k <$> use stPSDict) >>= \case
      Nothing -> error "impossible: parse state without number"
      Just (snew, is0)  -> do
        -- Lookaheads are already updated by convert.
        -- -- Update the lookaheads
        -- is <- do
        --   let is2 = is <> is0
        --   if is2 == is0 then return is0 else do
        --     modifying stPSDict $ Map.insert k (snew, is2)
        --     return is2
        -- Compute successors of snew.
        let sucs = Map.toList $ successors grm is
        -- Register the successors (if not known yet).
        (news, sucs') <- List.unzip <$> mapM convert sucs
        -- Compute goto actions for state snew.
        let fromSymbol (Term t, a) = Left  (t, a)
            fromSymbol (NT x  , a) = Right (x, a)
        let (shifts0, gotos0) = partitionEithers $ map fromSymbol sucs'
        -- Equip the state snew with its goto actions.
        unless (null gotos0) $ do
          let gotos   = IntMap.fromList gotos0
          modifying (stIPT . iptGoto) $ IntMap.insertWith IntMap.union snew gotos
        -- Compute shift and reduce actions of snew.
        let shifts  = Map.fromList $ map (\ (t,s) -> (t, shiftAction s)) shifts0
        let reduces = reductions is
        let actions = (shiftActions shifts <> reduces)
        unless (actions == mempty) $ do
        -- Equip the state snew with its shift/reduce actions.
          modifying (stIPT . iptSR) $ IntMap.insertWith (<>) snew actions
        -- Add the new states to the worklist and continue
        loop $ catMaybes news ++ worklist

  -- Register a parse state and decide whether we have to process it.
  convert :: (a, ParseState' r t) -> State (PTGenState' r t) (Maybe (ParseState' r t), (a, PState))
  convert (a, is) = do
    let k = lr0state is
    snew <- use stNext
    (Map.lookup k <$> use stPSDict) >>= \case
      -- Parse state has already been visited.  However, lookahead info might need update.
      Just (s, is0) -> do
        -- Combine old an new lookahead info.
        let is' = is <> is0
        if is' == is0 then return (Nothing, (a, s)) else do
          -- If something changed, update the lookahead info.
          -- Also, we will need to process this state again.
          modifying stPSDict $ Map.insert k (s, is')
          return (Just is', (a, s))
      -- New parse state.
      Nothing -> do
        -- Increase parse state counter.
        modifying stNext succ
        -- Save updated dictionary.
        modifying stPSDict $ Map.insert k (snew, is) -- (const dict')
        return (Just is, (a, snew))

-- | Shift over reduce.
--   First reduce action out of several ones.

chooseAction :: ISRAction' r t -> Maybe (Either PState (Rule' r t))
chooseAction (ISRAction (Just s) rs) = Just (Left s)
chooseAction (ISRAction Nothing  rs) = Right <$> do listToMaybe $ Set.toList rs

-- | Construct the extensional parse table.
constructParseTable' :: forall x r t. (Ord r, Ord t) => IPT' r t -> ParseTable' r t PState
constructParseTable' (IPT sr goto) = ParseTable tabSR tabGoto tabInit
  where
  tabSR s Nothing  = chooseAction =<< do view iactEof <$> IntMap.lookup s sr
  tabSR s (Just t) = chooseAction =<< Map.lookup t =<< do view iactTerm <$> IntMap.lookup s sr
  tabGoto s x = IntMap.lookup x =<< IntMap.lookup s goto
  tabInit = 0

-- | Construct the extensional parse table.
constructParseTable :: forall x r t. (Ord r, Ord t) => EGrammar' x r t -> ParseTable' r t PState
constructParseTable = constructParseTable' . ptGen

-- | Add rule @%start -> S@ for new start symbol.
addNewStart :: x -> r -> EGrammar' x r t -> EGrammar' x r t
addNewStart x r (EGrammar grm start fs) = EGrammar (add grm) newstart fs
  where
  add = over grmNTDefs $ IntMap.insert newstart $ NTDef x $ [Alt r $ Form [NT start]]
  newstart = -1
