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
allTransitions = Map.map inOut transitionSpec
  where
    inOut :: [TxConstraint False script] -> (Maybe (Spine (State script)), Maybe (Spine (State script)))
    inOut cs = (transitionInStateSpine cs, transitionOutStateSpine cs)

transitionInStateSpine ::
  (CEMScript script) =>
  [TxConstraint False script] ->
  Maybe (Spine (State script))
transitionInStateSpine = onlyTransitionStateSpine In

transitionOutStateSpine ::
  (CEMScript script) =>
  [TxConstraint False script] ->
  Maybe (Spine (State script))
transitionOutStateSpine = onlyTransitionStateSpine Out

onlyTransitionStateSpine ::
  (CEMScript script) =>
  UtxoKind ->
  [TxConstraint False script] ->
  Maybe (Spine (State script))
onlyTransitionStateSpine kind spec = case transitionStateSpines kind spec of
  [x] -> Just x
  [] -> Nothing
  _ ->
    error
      "Transition should not have more than one SameScript In/Out/InRef constraint"

-- | Get all states for a transition constraints based on a utxo kind.
transitionStateSpines ::
  forall script.
  (CEMScript script) =>
  UtxoKind ->
  [TxConstraint False script] ->
  [Spine (State script)]
transitionStateSpines kind spec = concat $ map ownUtxoState spec
  where
    ownUtxoState constr = case constr of
      Utxo kind' (SameScript (MkSameScriptArg state)) _ -> [parseSpine state | kind' == kind]
      If _ t e -> ownUtxoState t <> ownUtxoState e
      MatchBySpine _ caseSwitch -> foldMap ownUtxoState (Map.elems caseSwitch)
      _ -> []

    parseSpine :: ConstraintDSL script (State script) -> Spine (State script)
    parseSpine (Pure state) = getSpine state
    parseSpine (UnsafeOfSpine spine _) = spine
    parseSpine (UnsafeUpdateOfSpine _ spine _) = spine
    -- This should not happen anymore due to use of 'SameScriptArg'
    -- and smart constructors.
    parseSpine _ = error "SameScript is too complex to statically know its spine"

-- -----------------------------------------------------------------------------
-- Some preliminary checks
-- -----------------------------------------------------------------------------

-- Checks are based on this pseudo-lattice ordering.
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

{- | Performs some preliminary checks over the CEM script specification:
* there is only one initial transition
* every transition has zero or one `In` state
-}
preProcessForOnChainCompilation ::
  (CEMScript script, Show a) =>
  Map.Map a [TxConstraint False script] ->
  Map.Map a [TxConstraint False script]
preProcessForOnChainCompilation spec =
  if length initialTransitions == 1
    then
      let
        -- PM relies on `error` inside transitionInStateSpine
        !_ = map transitionInStateSpine $ Map.elems spec
       in
        spec
    else
      error $
        "CEMScript must have exactly one initial transition, "
          <> "while there are many ones: "
          <> ppShowList initialTransitions
  where
    initialTransitions = filter (isInitial . snd) (Map.toList spec)

    isInitial :: [TxConstraint resolved script] -> Bool
    isInitial constrs =
      not (maybeHasSameScriptFanOfKind In)
        && maybeHasSameScriptFanOfKind Out
      where
        maybeHasSameScriptFanOfKind kind =
          any ((/= No) . isSameScriptOfKind kind) constrs

    isSameScriptOfKind :: UtxoKind -> TxConstraint resolved script -> CheckResult
    isSameScriptOfKind xKind constr = case constr of
      Utxo kind (SameScript _) _ ->
        if kind == xKind then Yes else No
      If _ t e -> min (recur t) (recur e)
      MatchBySpine _ caseSwitch ->
        minimum $ map recur (Map.elems caseSwitch)
      _ -> No
      where
        recur = isSameScriptOfKind xKind
