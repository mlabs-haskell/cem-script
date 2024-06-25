{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.CEM.DSL where

import Prelude

import Data.Map qualified as Map

import Data.Text (pack, unpack)
import Text.Show.Pretty (ppShowList)

import Cardano.CEM
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Spine (HasSpine (..))
import PlutusLedgerApi.V1 (PubKeyHash)

-- Generic check datatypes

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

-- Checks

sameScriptStateSpinesOfKind ::
  forall script.
  (CEMScript script) =>
  TxFanKind ->
  TxConstraint False script ->
  [Spine (State script)]
sameScriptStateSpinesOfKind xKind constr = case constr of
  TxFan kind (SameScript state) _ ->
    if kind == xKind then [parseSpine state] else []
  If _ t e -> recur t <> (recur e)
  MatchBySpine _ caseSwitch ->
    foldMap recur (Map.elems caseSwitch)
  _ -> []
  where
    recur = sameScriptStateSpinesOfKind xKind
    parseSpine ::
      ConstraintDSL script (State script) -> Spine (State script)
    parseSpine (Pure state) = getSpine state
    parseSpine (UnsafeOfSpine spine _) = spine
    parseSpine (UnsafeUpdateOfSpine _ spine _) = spine
    -- FIXME: yet another not-properly DSL type encoded place
    parseSpine _ =
      error "SameScript is too complex to statically know its spine"

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

transitionInState ::
  [TxConstraint True script] -> Maybe (State script)
transitionInState c = listToMaybe $ mapMaybe f c
  where
    f (TxFan In (SameScript state) _) = Just state
    f _ = Nothing

transitionStateSpines :: (CEMScript script) => TxFanKind -> [TxConstraint False script] -> [Spine (State script)]
transitionStateSpines kind spec = concat $ map (sameScriptStateSpinesOfKind kind) spec

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

maybeIsCreator :: [TxConstraint resolved script] -> Bool
maybeIsCreator constrs =
  not (maybeHasSameScriptFanOfKind In)
    && maybeHasSameScriptFanOfKind Out
  where
    maybeHasSameScriptFanOfKind kind =
      any ((/= No) . isSameScriptOfKind kind) constrs

getMainSigner :: [TxConstraint True script] -> PubKeyHash
getMainSigner cs = case mapMaybe f cs of
  [pkh] -> pkh
  _ ->
    error
      "Transition should have exactly one MainSignerCoinSelection constraint"
  where
    f (MainSignerNoValue pkh) = Just pkh
    f (MainSignerCoinSelect pkh _ _) = Just pkh
    f _ = Nothing

-- FIXME: support for reusing error message between transitions in codes
-- FIXME: add golden tests
parseErrorCodes ::
  String ->
  Map.Map k [TxConstraint resolved script] ->
  [(String, String)]
parseErrorCodes prefix spec =
  go [] $ concat $ Map.elems spec
  where
    go table (constraint : rest) =
      case constraint of
        Error message ->
          let
            code = prefix <> show (length table)
           in
            go ((code, unpack message) : table) rest
        If _ t e -> go table (t : e : rest)
        (MatchBySpine _ caseSwitch) ->
          go table (Map.elems caseSwitch ++ rest)
        _ -> go table rest
    go table [] = reverse table

substErrors ::
  Map.Map String String ->
  Map.Map k [TxConstraint a b] ->
  Map.Map k [TxConstraint a b]
substErrors mapping spec =
  Map.map (map go) spec
  where
    go (Error message) = Error $ pack $ mapping Map.! unpack message
    go (If x t e) = If x (go t) (go e)
    go (MatchBySpine @a @b @c s caseSwitch) =
      MatchBySpine @a @b @c s $ Map.map go caseSwitch
    go x = x

-- FIXME: check MainSignerCoinSelect, ...

-- | Checking for errors and normaliing
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
