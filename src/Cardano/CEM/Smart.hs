{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Smart constructors fir DSL/constraints
module Cardano.CEM.Smart where

import Cardano.CEM.DSL
import Data.Map (Map)
import Data.Singletons.TH
import Data.Spine (HasPlutusSpine, HasSpine (..), spineFieldsNum)
import Data.Text (Text)
import Plutarch ((#))
import Plutarch.Extras
import Plutarch.LedgerApi (KeyGuarantees (..))
import Plutarch.LedgerApi.Value
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Prelude (
  PPartialOrd (..),
  pnot,
  (#&&),
 )
import PlutusLedgerApi.V2 (PubKeyHash, ToData (..), Value)
import Prelude hiding (Eq, error)
import Prelude qualified (Eq, error)

-- -----------------------------------------------------------------------------
-- Helpers to be used in actual definitions

-- General Ask

askC ::
  forall (var :: CVar) script.
  (SingI var) =>
  ConstraintDSL script (CVarType var script)
askC = Ask @var @_ @script (Proxy :: Proxy var)

-- Specific Asks TODO: rename

ctxParams :: ConstraintDSL script (Params script)
ctxTransition :: ConstraintDSL script (Transition script)
ctxParams = askC @CParams
ctxTransition = askC @CTransition
ctxState :: ConstraintDSL script (State script)
ctxState = askC @CState
ctxComp :: ConstraintDSL script (TransitionComp script)
ctxComp = askC @CComp

-- Pure

lift ::
  (Show value', ToData value') =>
  value' ->
  ConstraintDSL script value'
lift = Pure

cMkAdaOnlyValue ::
  ConstraintDSL script Integer -> ConstraintDSL script Value
cMkAdaOnlyValue = LiftPlutarch pMkAdaOnlyValue

cEmptyValue :: ConstraintDSL script Value
cEmptyValue = cMkAdaOnlyValue $ lift 0

cMinLovelace :: ConstraintDSL script Value
cMinLovelace = cMkAdaOnlyValue $ lift 3_000_000

-- TODO: These both are updates
cOfSpine ::
  (HasPlutusSpine datatype) =>
  Spine datatype ->
  [RecordSetter (ConstraintDSL script) datatype] ->
  ConstraintDSL script datatype
-- FIXME: should it be ordered?
cOfSpine spine setters =
  if toInteger (length setters) == toInteger (spineFieldsNum spine)
    then UnsafeOfSpine spine setters
    else
      Prelude.error $
        "OfSpine got less setters when number of fields ("
          <> show (spineFieldsNum spine)
          <> ")"

withState ::
  (HasPlutusSpine (State script)) =>
  Spine (State script) ->
  [RecordSetter (ConstraintDSL script) (State script)] ->
  ConstraintDSL script (State script)
withState = cOfSpine

nullarySpine :: (HasPlutusSpine datatype) => Spine datatype -> ConstraintDSL script datatype
nullarySpine spine = cOfSpine spine []

-- | Specialization for State
withNullaryState ::
  (CEMScript script, HasPlutusSpine (State script)) =>
  Spine (State script) ->
  ConstraintDSL script (State script)
withNullaryState spine = cOfSpine spine []

cUpdateOfSpine ::
  (HasPlutusSpine datatype) =>
  ConstraintDSL script datatype ->
  Spine datatype ->
  [RecordSetter (ConstraintDSL script) datatype] ->
  ConstraintDSL script datatype
cUpdateOfSpine = UnsafeUpdateOfSpine

cUpdateOfSpine' ::
  (HasPlutusSpine datatype) =>
  ConstraintDSL script datatype ->
  Spine datatype ->
  ConstraintDSL script datatype
cUpdateOfSpine' orig spine = UnsafeUpdateOfSpine orig spine []

-- | TODO: notes on how states are compared
inState ::
  (HasPlutusSpine (State script)) =>
  Spine (State script) ->
  ConstraintDSL script (State script)
inState spine = UnsafeUpdateOfSpine ctxState spine []

(@==) ::
  (Prelude.Eq x) => ConstraintDSL script x -> ConstraintDSL script x -> ConstraintDSL script Bool
(@==) = Eq

(@<=) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@<=) = LiftPlutarch2 (#<=)

(@<) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@<) = LiftPlutarch2 (#<)

(@>=) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@>=) = flip (@<=)

(@>) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@>) = flip (@<)

(@<>) ::
  ConstraintDSL script Value ->
  ConstraintDSL script Value ->
  ConstraintDSL script Value
(@<>) =
  LiftPlutarch2 @(PValue 'Unsorted 'NonZero) @_ @_ merge
  where
    merge x y =
      pforgetSorted $
        (<>)
          (passertSorted # x)
          (passertSorted # y)

infixr 3 @&&

(@&&) ::
  ConstraintDSL script Bool ->
  ConstraintDSL script Bool ->
  ConstraintDSL script Bool
(@&&) = LiftPlutarch2 (#&&)

cNot :: ConstraintDSL script Bool -> ConstraintDSL script Bool
cNot = LiftPlutarch pnot

-- TxConstraint utils

-- | Check constraint only in the offchain
offchainOnly :: TxConstraint False script -> TxConstraint False script
offchainOnly c = If IsOnChain Noop c

byFlagError ::
  ConstraintDSL script Bool -> Text -> TxConstraint False script
byFlagError flag message = If flag (Error message) Noop

-- tx inputs/outputs
input ::
  forall script.
  () =>
  Utxo False script ->
  DSLValue False script Value ->
  TxConstraint False script
input = Utxo In

refInput ::
  forall script.
  () =>
  Utxo False script ->
  DSLValue False script Value ->
  TxConstraint False script
refInput = Utxo InRef

output ::
  forall script.
  () =>
  Utxo False script ->
  DSLValue False script Value ->
  TxConstraint False script
output = Utxo Out

userUtxo ::
  forall (resolved :: Bool) script.
  DSLValue resolved script PubKeyHash ->
  Utxo resolved script
userUtxo = UserAddress

ownUtxo ::
  forall (resolved :: Bool) script.
  DSLValue resolved script (State script) ->
  Utxo resolved script
ownUtxo = SameScript . MkSameScriptArg

signedBy ::
  forall (resolved :: Bool) script.
  DSLValue resolved script PubKeyHash ->
  TxConstraint resolved script
signedBy = MainSignerNoValue

spentBy ::
  forall (resolved :: Bool) script.
  DSLValue resolved script PubKeyHash ->
  DSLValue resolved script Value ->
  DSLValue resolved script Value ->
  TxConstraint resolved script
spentBy = MainSignerCoinSelect

noop :: forall (resolved :: Bool) script. TxConstraint resolved script
noop = Noop

error ::
  forall (resolved :: Bool) script.
  Text ->
  TxConstraint resolved script
error = Error

match ::
  forall (resolved :: Bool) script datatype.
  (HasPlutusSpine datatype) =>
  DSLPattern resolved script datatype ->
  Map (Spine datatype) (TxConstraint resolved script) ->
  TxConstraint resolved script
match = MatchBySpine

if' ::
  forall (resolved :: Bool) script.
  DSLPattern resolved script Bool ->
  TxConstraint resolved script ->
  TxConstraint resolved script ->
  TxConstraint resolved script
if' = If

eq' ::
  forall x script.
  (Prelude.Eq x) =>
  ConstraintDSL script x ->
  ConstraintDSL script x ->
  ConstraintDSL script Bool
eq' = Eq
