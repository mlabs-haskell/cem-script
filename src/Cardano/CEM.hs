{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoPolyKinds #-}

module Cardano.CEM where

import PlutusTx.Prelude
import Prelude (Show)
import Prelude qualified

import Data.Data (Proxy)
import Data.Map qualified as Map

-- Plutus imports
import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (
  ToData (..),
  Value,
 )
import PlutusTx.Show.TH (deriveShow)

-- Project imports
import Cardano.CEM.Stages
import Data.Spine

-- | This is different ways to specify address
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

data TxFanFilter script = MkTxFanFilter
  { address :: AddressSpec
  , rest :: TxFanFilter' script
  }
  deriving stock (Show, Prelude.Eq)

data TxFanFilter' script
  = Anything
  | -- | To be used via `bySameCem`
    UnsafeBySameCEM BuiltinData
  | ByDatum BuiltinData
  deriving stock (Show, Prelude.Eq)

{-# INLINEABLE bySameCEM #-}

-- | Constraint enforcing state of script mentioning this constraint
bySameCEM ::
  (ToData (State script), CEMScript script) =>
  State script ->
  TxFanFilter' script
bySameCEM = UnsafeBySameCEM . toBuiltinData

-- TODO: use natural numbers
data Quantor = Exist Integer | SumValueEq Value
  deriving stock (Show)

data TxFanKind = In | InRef | Out
  deriving stock (Prelude.Eq, Prelude.Show)

data TxFanConstraint script = MkTxFanC
  { txFanCKind :: TxFanKind
  , txFanCFilter :: TxFanFilter script
  , txFanCQuantor :: Quantor
  }
  deriving stock (Show)

-- Main API

class
  ( HasSpine (Transition script)
  , HasSpine (State script)
  , Stages (Stage script)
  , Show (Stage script)
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
  type Transition script = transition | transition -> script

  -- | Each kind of Transition has statically associated Stage
  -- from/to `State`s spines
  transitionStage ::
    Proxy script ->
    Map.Map
      (Spine (Transition script))
      ( Stage script
      , Maybe (Spine (State script))
      , Maybe (Spine (State script))
      )

  -- This functions define domain logic
  transitionSpec ::
    Params script ->
    Maybe (State script) ->
    Transition script ->
    Either BuiltinString (TransitionSpec script)

data TransitionSpec script = MkTransitionSpec
  { constraints :: [TxFanConstraint script]
  , -- List of additional signers (in addition to one required by TxIns)
    signers :: [PubKeyHash]
  }
  deriving stock (Show)

-- | List of all signing keys required for transition spec
getAllSpecSigners :: TransitionSpec script -> [PubKeyHash]
getAllSpecSigners spec = signers spec ++ txInPKHs
  where
    txInPKHs = mapMaybe getPubKey $ filter ((Prelude.== In) . txFanCKind) $ constraints spec
    getPubKey c = case address (txFanCFilter c) of
      ByPubKey key -> Just key
      _ -> Nothing

{- | Static part of CEMScript datum.
Datatype is actually used only by off-chain code due to Plutus limitations.
-}
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

-- TH deriving done at end of file for GHC staging reasons

deriveShow ''TxFanKind
deriveShow ''TxFanFilter'
