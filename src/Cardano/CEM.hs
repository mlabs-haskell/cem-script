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
import PlutusLedgerApi.V2 (ToData (..), Value)
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

-- "Tx Fan" - is transaction input or output 

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
-- | How many tx fans should satify a 'TxFansConstraint'
data Quantifier = Exist Integer | SumValueEq Value
  deriving stock (Show)

data TxFanKind = In | InRef | Out
  deriving stock (Prelude.Eq, Prelude.Show)

-- | A constraint on Tx inputs or Outputs.
data TxFansConstraint script = MkTxFansC
  { txFansCKind :: TxFanKind -- is constraint applies strictly on inputs or on outputs
  , txFansCFilter :: TxFanFilter script -- constraint on a single tx fan
  , txFansCQuantor :: Quantifier -- how much fans are required to match
  }
  deriving stock (Show)

-- Main API

-- FIXME: move IsData here (now it breaks Plutus compilation)
type DefaultConstraints datatype =
  ( Prelude.Eq datatype
  , Prelude.Show datatype
  )

{- | All associated types for `CEMScript`
They are separated to simplify TH deriving
-}
class CEMScriptTypes script where
  -- \| `Params` is immutable part of script Datum,
  -- \| it should be used to encode all
  type Params script = params | params -> script

  -- | `Stage` is datatype encoding all `Interval`s specified by script.
  -- | `Stage` logic is encoded by separate `Stages` type class.
  -- | It have separate `StageParams` datatype,
  -- | which is stored immutable in script Datum as well.
  type Stage script

  type Stage script = SingleStage

  -- | `State` is changing part of script Datum.
  -- | It is in
  type State script = params | params -> script

  -- | Transitions for deterministic CEM-machine
  type Transition script = transition | transition -> script

class
  ( HasSpine (Transition script)
  , HasSpine (State script)
  , Stages (Stage script)
  , DefaultConstraints (Stage script)
  , DefaultConstraints (Transition script)
  , DefaultConstraints (State script)
  , DefaultConstraints (Params script)
  , DefaultConstraints (StageParams (Stage script))
  , CEMScriptTypes script
  ) =>
  CEMScript script
  where
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
  { constraints :: [TxFansConstraint script]
  , -- List of additional signers (in addition to one required by TxIns)
    signers :: [PubKeyHash]
  }
  deriving stock (Show)

-- | List of all signing keys required for transition spec
getAllSpecSigners :: TransitionSpec script -> [PubKeyHash]
getAllSpecSigners spec = signers spec ++ txInPKHs
  where
    txInPKHs = mapMaybe getPubKey $ filter ((Prelude.== In) . txFansCKind) $ constraints spec
    getPubKey c = case address (txFansCFilter c) of
      ByPubKey key -> Just key
      _ -> Nothing

{- | Static part of CEMScript datum.
Datatype is actually used only by off-chain code due to Plutus limitations.
-}
data CEMParams script = MkCEMParams
  { scriptParams :: Params script
  , stagesParams :: StageParams (Stage script)
  }

deriving stock instance (CEMScript script) => (Show (CEMParams script))
deriving stock instance (CEMScript script) => (Prelude.Eq (CEMParams script))

-- FIXME: documentation
type CEMScriptDatum script =
  (StageParams (Stage script), Params script, State script)

-- TH deriving done at end of file for GHC staging reasons

deriveShow ''TxFanKind
deriveShow ''TxFanFilter'
