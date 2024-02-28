{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoPolyKinds #-}

module Cardano.CEM where

import PlutusTx.IsData (toData)
import PlutusTx.Prelude
import Prelude (Show)
import Prelude qualified

import Data.Data (Proxy)
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (
  BuiltinData (..),
  Data (..),
  FromData (..),
  ToData (..),
  Value,
  fromData,
 )
import PlutusTx.Show.TH (deriveShow)

import Cardano.CEM.Stages
import Data.Spine

data AddressSpec
  = ByAddress Address
  | ByPubKey PubKeyHash
  | BySameScript
  deriving stock (Show, Prelude.Eq)

addressSpecToAddress :: Address -> AddressSpec -> Address
addressSpecToAddress ownAddress addressSpec = case addressSpec of
  ByAddress address -> address
  ByPubKey pubKey -> pubKeyHashAddress pubKey
  BySameScript -> ownAddress

data TxFanFilter script = MkTxFanFilter
  { address :: AddressSpec
  , rest :: TxFanFilter' script
  }
  deriving stock (Show, Prelude.Eq)

data TxFanFilter' script
  = Anything
  | -- TODO
    BySameCEM BuiltinData
  | ByDatum BuiltinData
  deriving stock (Show, Prelude.Eq)

{-# INLINEABLE bySameCEM #-}
-- TODO: rename
bySameCEM ::
  (ToData (State script), CEMScript script) =>
  State script ->
  TxFanFilter' script
bySameCEM = BySameCEM . toBuiltinData

-- TODO: use natural numbers
data Quantor = Exist Integer | SumValueEq Value

data TxFanKind = In | InRef | Out
  deriving stock (Prelude.Eq, Prelude.Show)

data TxFanConstraint script = MkTxFanC
  { txFanCKind :: TxFanKind
  , txFanCFilter :: TxFanFilter script
  , txFanCQuantor :: Quantor
  }

-- Main API

class
  ( HasSpine (Transition script)
  , HasSpine (State script)
  , Stages (Stage script)
  ) =>
  CEMScript script
  where
  -- | `Params` is immutable part of script Datum,
  -- | it should be used to encode all
  type Params script = params | params -> script

  -- | `Stage` is datatype encoding all `Interval`s specified by script.
  -- | `Stage` logic is encoded by separate `Stages` type class.
  -- | It have separate `StageParams` datatype,
  -- | which is stored immutable in script Datum as well.
  type Stage script

  -- | `State` is changing part of script Datum.
  -- | It is in
  type State script = params | params -> script

  -- | Transitions for deterministic CEM-machine
  type Transition script = transtion | transtion -> script

  -- | Each kind of Transition has statically associated Stage and State spine
  transitionStage ::
    Proxy script ->
    Map.Map
      (Spine (Transition script))
      (Stage script, Maybe (Spine (State script)))

  -- This functions define domain logic
  transitionSpec ::
    Params script ->
    Maybe (State script) ->
    Transition script ->
    Either BuiltinString (TransitionSpec script)

data TransitionSpec script = MkTransitionSpec
  { constraints :: [TxFanConstraint script]
  , signers :: [PubKeyHash]
  }

data CEMParams script = MkCEMParams
  { scriptParams :: Params script
  , stagesParams :: StageParams (Stage script)
  }

deriving stock instance
  ( Show (Params script)
  , (Show (StageParams (Stage script)))
  ) =>
  (Show (CEMParams script))

deriving stock instance
  ( Prelude.Eq (Params script)
  , (Prelude.Eq (StageParams (Stage script)))
  ) =>
  (Prelude.Eq (CEMParams script))

-- TODO: doc
type CEMScriptDatum script =
  (StageParams (Stage script), Params script, State script)

-- Bunch of conditional `IsData` instances
-- Plutus TH utils does not work for that case

instance
  (ToData (Params script), ToData (StageParams (Stage script))) =>
  ToData (CEMParams script)
  where
  toBuiltinData (MkCEMParams {..}) =
    BuiltinData $ List [toData scriptParams, toData stagesParams]

instance
  (FromData (Params script), FromData (StageParams (Stage script))) =>
  FromData (CEMParams script)
  where
  fromBuiltinData (BuiltinData (List [scriptParams, stagesParams])) =
    MkCEMParams <$> fromData scriptParams <*> fromData stagesParams
  fromBuiltinData _ = Nothing

-- TH deriving done at end of file for GHC staging reasons

deriveShow ''TxFanKind
deriveShow ''TxFanFilter'
