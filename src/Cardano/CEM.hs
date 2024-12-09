{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoPolyKinds #-}

module Cardano.CEM (
  -- * Main API
  CEMScript (..),
  CEMScriptTypes (..),

  -- * Stages
  Stages (..),
  SingleStage (..),
  SingleStageParams (..),

  -- * Addresses
  AddressSpec (..),
  addressSpecToAddress,

  -- * Datum
  CEMScriptDatum,
  CEMParams (..),

  -- * "TxFans"
  TxFanKind (..),
  TxFanFilter (..),
  FilterDatum (..),
  bySameCEM,
  Quantifier (..),
  TxFansConstraint (..),

  -- * Transition specification
  TransitionSpec (..),
  getAllSpecSigners,
)
where

import PlutusTx.Prelude (
  BuiltinData,
  BuiltinString,
  Either,
  Integer,
  Maybe (..),
  filter,
  mapMaybe,
  ($),
  (++),
  (.),
 )
import Prelude (Show)
import Prelude qualified

import Data.Data (Proxy)
import Data.Map qualified as Map

-- Plutus imports
import PlutusLedgerApi.V1 (
  Address,
  Interval (..),
  POSIXTime (..),
  PubKeyHash,
  ToData (..),
  Value,
  always,
 )
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusTx qualified
import PlutusTx.Show.TH qualified

-- Project imports
import Data.Spine

-- -----------------------------------------------------------------------------
-- Main API
-- -----------------------------------------------------------------------------

{- | The core type class that is used to define a state machine.
     Skip constraints and go to method definitions to get it.
-}
class
  ( HasSpine (Transition script)
  , HasSpine (State script)
  , Stages (Stage script)
  , CEMScriptTypes script
  , EqShow (Stage script)
  , EqShow (Transition script)
  , EqShow (State script)
  , EqShow (Params script)
  , EqShow (StageParams (Stage script))
  ) =>
  CEMScript script
  where
  -- | This is the map of all possible machine 'Transition's.
  --          This statically associates every 'Transition' with
  --          a 'Stage' through source/target 'State's.
  transitionStage ::
    Proxy script ->
    Map.Map
      (Spine (Transition script))
      ( Stage script
      , Maybe (Spine (State script)) -- source 'State'
      , Maybe (Spine (State script)) -- target 'State'
      )

  -- | This function defines domain logic of a CEM Script by
  -- providing 'TransitionSpec' for possible 'Transition's.
  transitionSpec ::
    Params script ->
    Maybe (State script) ->
    Transition script ->
    Either BuiltinString (TransitionSpec script)

type EqShow datatype =
  ( Prelude.Eq datatype
  , Prelude.Show datatype
  -- Shoul we add IsData here? (now it breaks Plutus compilation)
  )

{- | All associated types for 'CEMScript' class defined separately to simplify
     TH deriving.
-}
class CEMScriptTypes script where
  -- | Params - immutable part of script Datum
  type Params script = params | params -> script

  -- | `Stage` is datatype encoding all `Interval`s specified by script.
  --        `Stage` logic is encoded by separate `Stages` type class.
  --         It have separate `StageParams` datatype,
  --         which is stored in the immutable part of Datum as well.
  type Stage script

  -- | The defaulf type is 'SingleStage' that defines one time span where
  --        the script can operate.
  type Stage script = SingleStage

  -- | `State` is stored in the mutable part of script's Datum
  --        and carries the current state of the machine.
  type State script = params | params -> script

  -- | This type defines possible transitions of the machine.
  type Transition script = transition | transition -> script

-- -----------------------------------------------------------------------------
-- Stages
-- -----------------------------------------------------------------------------

-- | CEM Script can define time spans in which they operate called 'Stage's.
class Stages stage where
  type StageParams stage = params | params -> stage

  -- | Calculates the interval based on the stage parametes
  stageToOnChainInterval :: StageParams stage -> stage -> Interval POSIXTime

-- | The simplest case when there is only one span.
data SingleStage = Always
  deriving stock (Prelude.Show, Prelude.Eq)

data SingleStageParams
  = NoSingleStageParams
  | AllowedInterval (Interval POSIXTime)
  deriving stock (Prelude.Show, Prelude.Eq)

instance Stages SingleStage where
  type StageParams SingleStage = SingleStageParams

  stageToOnChainInterval NoSingleStageParams Always = always
  stageToOnChainInterval (AllowedInterval interval) Always = interval

-- -----------------------------------------------------------------------------
-- Addresses
-- -----------------------------------------------------------------------------

-- | This is different ways to specify address within the context of CEM Script.
data AddressSpec
  = ByAddress Address
  | ByPubKey PubKeyHash
  | BySameScript
  deriving stock (Show, Prelude.Eq)

{-# INLINEABLE addressSpecToAddress #-}
addressSpecToAddress :: Address -> AddressSpec -> Address
addressSpecToAddress ownAddress addressSpec = case addressSpec of
  ByAddress address -> address
  ByPubKey pubKey -> pubKeyHashAddress pubKey
  BySameScript -> ownAddress

-- -----------------------------------------------------------------------------
-- Datums
-- -----------------------------------------------------------------------------

{- | Static part of CEMScript datum.
     Datatype is actually used only by off-chain code due to Plutus limitations.
     Real datums use 'CEMScriptDatum', see link below.
-}
data CEMParams script = MkCEMParams
  { scriptParams :: Params script
  , stagesParams :: StageParams (Stage script)
  }

deriving stock instance (CEMScript script) => (Show (CEMParams script))
deriving stock instance (CEMScript script) => (Prelude.Eq (CEMParams script))

-- This can't be made anything than a tuple typealias because
-- of the Plutus compiler limitations:
-- https://github.com/IntersectMBO/plutus/issues/5769
type CEMScriptDatum script =
  (StageParams (Stage script), Params script, State script)

-- -----------------------------------------------------------------------------
-- "TxFans"
-- -----------------------------------------------------------------------------

{- | Within CEM Script we use the term tf fan to refer to transaction inputs and
outputs. 'TxFanKind' introduces possible variants of "funs".
-}
data TxFanKind
  = -- | regular input
    In
  | -- | ref input
    InRef
  | -- | output
    Out
  deriving stock (Prelude.Eq, Prelude.Show)

-- | Constraint on a single tx fan
data TxFanFilter script = MkTxFanFilter
  { address :: AddressSpec
  , datumFilter :: FilterDatum script
  }
  deriving stock (Show, Prelude.Eq)

-- | Tx Fan matches by
data FilterDatum script
  = AnyDatum
  | -- | To be used via `bySameCem`
    --   Basically means "make new 'CEMScriptDatum' containing this State
    --   and then use with 'ByDatum'"
    UnsafeBySameCEM (AsData (State script))
  | ByDatum (AsData (CEMScriptDatum script))
  deriving stock (Show, Prelude.Eq)

-- This could only be a type alias
-- https://github.com/IntersectMBO/plutus/issues/5769
type AsData a = BuiltinData

{-# INLINEABLE bySameCEM #-}

-- | Constraint enforcing state of script mentioning this constraint
bySameCEM ::
  (ToData (State script), CEMScript script) =>
  State script ->
  FilterDatum script
bySameCEM = UnsafeBySameCEM . toBuiltinData

-- | How many tx fans should satify a 'TxFansConstraint'
data Quantifier
  = ExactlyNFans Integer -- Here we'd better use natural numbers
  | FansWithTotalValueOfAtLeast Value
  deriving stock (Show)

-- | A constraint on Tx inputs or Outputs.
data TxFansConstraint script = MkTxFansC
  { txFansCKind :: TxFanKind
  -- ^ is constraint applies to inputs/ref inputs/outputs?
  , txFansCFilter :: TxFanFilter script
  -- ^ constraint on a single tx fan
  , txFansCQuantor :: Quantifier
  -- ^ what subset of fans should match
  }
  deriving stock (Show)

-- -----------------------------------------------------------------------------
-- Transition specification
-- -----------------------------------------------------------------------------

data TransitionSpec script = MkTransitionSpec
  { constraints :: [TxFansConstraint script]
  , signers :: [PubKeyHash]
  -- ^ List of additional signers (in addition to required by constrainsts on inputs)
  }
  deriving stock (Show)

-- | List of all signing keys required for transition spec
getAllSpecSigners :: TransitionSpec script -> [PubKeyHash]
getAllSpecSigners spec = signers spec ++ txInPKHs
  where
    txInPKHs =
      mapMaybe getPubKey
        $ filter ((Prelude.== In) . txFansCKind)
        $ constraints spec
    getPubKey c = case address (txFansCFilter c) of
      ByPubKey key -> Just key
      _ -> Nothing

-- TH deriving done at end of file for GHC staging reasons
PlutusTx.Show.TH.deriveShow ''TxFanKind
PlutusTx.Show.TH.deriveShow ''FilterDatum

PlutusTx.unstableMakeIsData ''SingleStage
PlutusTx.unstableMakeIsData 'NoSingleStageParams
