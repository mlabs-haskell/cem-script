module Cardano.CEM.Examples.Escrow where

import PlutusTx qualified
import PlutusTx.Prelude

import PlutusLedgerApi.V1 (Address, Value)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)

import Cardano.CEM
import Cardano.CEM.Stages
import Cardano.CEM.OnChain (CEMScriptIsData, IsData)
import PlutusLedgerApi.V1.Value (AssetClass, assetClassValue)
import PlutusTx.IsData (FromData, ToData)
import qualified PlutusTx as Plutus

-- Generic escrows

-- TODO: move to Commons
class Escrow escrow where
  data EscrowParams escrow
  data EscrowUnlock escrow
  unlockConstraints ::
    EscrowParams escrow ->
    EscrowUnlock escrow ->
    Either BuiltinString (TransitionSpec (EscrowScript escrow))

newtype EscrowScript escrow = MkEscrowScript escrow

instance
  (Escrow escrow, IsData (EscrowUnlock escrow)) =>
  CEMScript (EscrowScript escrow)
  where
  type Stage (EscrowScript escrow) = SingleStage
  data Params (EscrowScript escrow) = MkEscrowParams (EscrowParams escrow)
  data State (EscrowScript escrow) = Locked
  data Transition (EscrowScript escrow) = UnLock (EscrowUnlock escrow)

  transitionSpec (MkEscrowParams params) (Just Locked) (UnLock unlock) =
    unlockConstraints params unlock


-- TODO
instance FromData (EscrowParams escrow) => FromData  (Params (EscrowScript escrow)) where
instance ToData (EscrowParams escrow) => ToData  (Params (EscrowScript escrow)) where
Plutus.unstableMakeIsData 'Locked
-- Plutus.unstableMakeIsData 'UnLock

-- Specific escrows

data UnboundedEscrow

instance Escrow UnboundedEscrow where
  data EscrowParams UnboundedEscrow = MkUnboundedEscrowParams
  data EscrowUnlock UnboundedEscrow = UnboundedEscrowUnlock
  unlockConstraints _ _ =
    Right $
      MkTransitionSpec
        { constraints = []
        , signers = []
        , stage = Always
        }

data UserLockedEscrow

instance Escrow UserLockedEscrow where
  data EscrowParams UserLockedEscrow = MkUserLockedState
    { unlockingUser :: PubKeyHash
    }
  data EscrowUnlock UserLockedEscrow = MkUserUnlock
  unlockConstraints state _ =
    Right $
      MkTransitionSpec
        { constraints = []
        , signers = [unlockingUser state]
        , stage = Always
        }

PlutusTx.unstableMakeIsData 'MkUserLockedState
PlutusTx.unstableMakeIsData 'MkUserUnlock

data TokenLockedEscrow

instance Escrow TokenLockedEscrow where
  data EscrowParams TokenLockedEscrow = MkTokenLockedState
    { unlockingToken :: AssetClass
    }
  data EscrowUnlock TokenLockedEscrow = MkTokenUnlock
    { unlocker :: PubKeyHash
    }
  unlockConstraints params (MkTokenUnlock {unlocker}) =
    Right $
      MkTransitionSpec
        { constraints =
            [ MkTxFanC
                InRef
                (MkTxFanFilter (ByPubKey unlocker) Anything)
                (SumValueEq singleToken)
                -- TODO: unlocker?
            ]
        , signers = [unlocker]
        , stage = Always
        }
    where
      singleToken = assetClassValue (unlockingToken params) 1

data HashLockedEscrow

instance Escrow HashLockedEscrow where
  data EscrowParams HashLockedEscrow = MkHashLockedState
    { secretHash :: BuiltinByteString
    }
  data EscrowUnlock HashLockedEscrow = MkHashLockedUnlock
    { secretValue :: BuiltinByteString
    }
  unlockConstraints state unlock =
    if blake2b_256 (secretValue unlock) == secretHash state
      then
        Right $
          MkTransitionSpec
            { constraints = []
            , signers = []
            , stage = Always
            }
      else Left "Wrong hash"

data FixedSwapEscrow

instance Escrow FixedSwapEscrow where
  data EscrowParams FixedSwapEscrow = MkSwapState
    { creator :: Address
    , lockedValue :: Value
    , toSwapValue :: Value
    }
  data EscrowUnlock FixedSwapEscrow = FixedSwapUnlock
    { swappingActor :: Address
    }
  unlockConstraints state unlock =
    Right $
      MkTransitionSpec
        { constraints =
            [ -- TODO: balance, need to sign?
              MkTxFanC Out (MkTxFanFilter (ByAddress (creator state)) Anything) (SumValueEq (toSwapValue state))
            , MkTxFanC Out (MkTxFanFilter (ByAddress (swappingActor unlock)) Anything) (SumValueEq (lockedValue state))
            ]
        , signers = []
        , stage = Always
        }

data FeeDistributionEscrow

instance Escrow FeeDistributionEscrow where
  data EscrowParams FeeDistributionEscrow = MkFeeDistributionParams
    { feeReceivers :: [Address]
    }

  -- TODO: explain
  data EscrowUnlock FeeDistributionEscrow = MkFeeDistributionUnlock
    { amountPerFeeReceiver :: Value
    }

  unlockConstraints params unlock =
    Right $
      MkTransitionSpec
        { constraints = map receiverConstraint $ feeReceivers params
        , signers = []
        , stage = Always
        }
    where
      receiverConstraint address =
        MkTxFanC
          Out
          (MkTxFanFilter (ByAddress address) Anything)
          (SumValueEq $ amountPerFeeReceiver unlock)
