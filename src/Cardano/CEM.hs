module Cardano.CEM where

import Prelude

import PlutusLedgerApi.V2 (
    Interval (..), POSIXTime (..), always, BuiltinByteString, Value)
import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)

-- Constraints and filters

data TxFanFilter =
    Anything
    | ByAddress Address
    | ByPubKey PubKeyHash
    | ByDatum BuiltinByteString
    | And [TxFanFilter]
    | Or [TxFanFilter]

data TxFanKind = In | InRef | Out | InAndOut

data TxFanConstraint =
    MkTxFanC TxFanKind TxFanFilter Value

-- Stages

-- This covers constraints on blockchain slot time,
-- used by both on- and off-chain code
class Stages stage where
    data StageParams stage
    stageToOnChainInterval :: StageParams stage -> stage -> Interval POSIXTime

-- Common

data SingleStage = Always

instance Stages SingleStage where
    data StageParams SingleStage = NoSingleStageParams
    stageToOnChainInterval _ Always = always

-- Main API

class Stages (Stage script) => CEMScript script where
    type Stage script
    data Params script
    -- This is in fact just a script Datum
    data State script
    -- Transitions for deterministic CEM-machine
    data Transition script

    -- This functions define domain logic
    transitionSpec :: Params script -> State script -> Transition script -> Either String (TransitionSpec script)

class CEMScript script => CEMScriptCompiled script where
    cemScriptStateST :: Params script -> State script -> TxFanConstraint

instance CEMScript script => CEMScriptCompiled script where
    cemScriptStateST =
        error "Should be implemented by Plutus compilation in M2"

data TransitionSpec script = MkTransitionSpec {
    сonstraints :: [TxFanConstraint],
    signers :: [PubKeyHash],
    stage :: Stage script
    }

