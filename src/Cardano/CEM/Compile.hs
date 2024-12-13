module Cardano.CEM.Compile (
  allTransitions,
  transitionInStateSpine,
  transitionOutStateSpine,
  transitionStateSpines,
  preProcessForOnChainCompilation,
) where

import Cardano.CEM.DSL
import Data.Map qualified as Map
import Data.Spine (HasSpine (..))
import Text.Show.Pretty (ppShowList)
import Prelude

allTransitions ::
  forall script.
  (CEMScript script) =>
  Map.Map
    (Spine (Transition script))
    ( Maybe (Spine (State script)) -- source 'State'
    , Maybe (Spine (State script)) -- target 'State'
    )
allTransitions = Map.map foo transitionSpec
  where
    foo :: [TxConstraint False script] -> (Maybe (Spine (State script)), Maybe (Spine (State script)))
    foo cs = (transitionInStateSpine cs, transitionOutStateSpine cs)

transitionInStateSpine ::
  (CEMScript script) =>
  [TxConstraint False script] ->
  Maybe (Spine (State script))
transitionInStateSpine spec = case transitionStateSpines In spec of
  [x] -> Just x
  [] -> Nothing
  _ ->
    error
      "Transition should not have more than one SameScript In constraint"

transitionOutStateSpine ::
  (CEMScript script) =>
  [TxConstraint False script] ->
  Maybe (Spine (State script))
transitionOutStateSpine spec = case transitionStateSpines Out spec of
  [x] -> Just x
  [] -> Nothing
  _ ->
    error
      "Transition should not have more than one SameScript In constraint"

transitionStateSpines :: (CEMScript script) => TxFanKind -> [TxConstraint False script] -> [Spine (State script)]
transitionStateSpines kind spec = concat $ map (sameScriptStateSpinesOfKind kind) spec
  where
    sameScriptStateSpinesOfKind ::
      forall script.
      (CEMScript script) =>
      TxFanKind ->
      TxConstraint False script ->
      [Spine (State script)]
    sameScriptStateSpinesOfKind xKind constr = case constr of
      TxFan kind (SameScript (MkSameScriptArg state)) _ -> [parseSpine state | kind == xKind]
      If _ t e -> recur t <> recur e
      MatchBySpine _ caseSwitch -> foldMap recur (Map.elems caseSwitch)
      _ -> []
      where
        recur = sameScriptStateSpinesOfKind xKind
        parseSpine ::
          ConstraintDSL script (State script) -> Spine (State script)
        parseSpine (Pure state) = getSpine state
        parseSpine (UnsafeOfSpine spine _) = spine
        parseSpine (UnsafeUpdateOfSpine _ spine _) = spine
        -- FIXME: yet another not-properly DSL type encoded place
        parseSpine _ = error "SameScript is too complex to statically know its spine"

-- FIXME: check MainSignerCoinSelect, ...

-- | Checking for errors and normalising
preProcessForOnChainCompilation ::
  (CEMScript script, Show a) =>
  Map.Map a [TxConstraint False script] ->
  Map.Map a [TxConstraint False script]
preProcessForOnChainCompilation spec =
  if length possibleCreators == 1
    then
      let
        -- FIXME: relies on `error` inside...
        !_ = map transitionInStateSpine $ Map.elems spec
       in
        spec
    else
      error $
        "CEMScript should have exactly 1 creating transition, "
          <> "while possible creators are "
          <> ppShowList possibleCreators
  where
    possibleCreators = filter (maybeIsCreator . snd) (Map.toList spec)

    maybeIsCreator :: [TxConstraint resolved script] -> Bool
    maybeIsCreator constrs =
      not (maybeHasSameScriptFanOfKind In)
        && maybeHasSameScriptFanOfKind Out
      where
        maybeHasSameScriptFanOfKind kind =
          any ((/= No) . isSameScriptOfKind kind) constrs

    isSameScriptOfKind :: TxFanKind -> TxConstraint resolved script -> CheckResult
    isSameScriptOfKind xKind constr = case constr of
      TxFan kind (SameScript _) _ ->
        if kind == xKind then Yes else No
      If _ t e -> min (recur t) (recur e)
      MatchBySpine _ caseSwitch ->
        minimum $ map recur (Map.elems caseSwitch)
      _ -> No
      where
        recur = isSameScriptOfKind xKind

-- | We have abstract interpretator at home
data CheckResult = Yes | No | Maybe
  deriving stock (Eq, Show)

opposite :: Ordering -> Ordering
opposite EQ = EQ
opposite LT = GT
opposite GT = LT

instance Ord CheckResult where
  compare Yes No = EQ
  compare Yes Maybe = GT
  compare No Maybe = GT
  compare Yes Yes = EQ
  compare No No = EQ
  compare Maybe Maybe = EQ
  compare x y = opposite $ compare y x
