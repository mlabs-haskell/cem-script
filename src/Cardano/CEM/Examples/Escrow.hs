module Cardano.CEM.Examples where

import Prelude

import Data.Proxy

import Cardano.CEM


-- Generic escrows

class Escrow script where
    data EscrowLockedState script
    data EscrowUnlock script
    unlockConstraints ::
        EscrowLockedState script -> EscrowUnlock script -> Maybe [TxFanConstraint]

instance Escrow script => CEMScript script where
    type Stage script = SingleStage
    data State script = Locked (EscrowLockedState script)
    data Transition script = UnLock (EscrowUnlock script)

    transitionSpec transition = case transition of
        UnLock escrowState -> MkTransitionSpec {
            transitionStates = (Locked, Nothing),
            сonstraints = unlockConstraints escrowState,
            transitionStage = Single
        }

-- Specific escrows

data UnboundedEscrow

instance Escrow UnboundedEscrow where
    data EscrowUnlock UnboundedEscrow
    unlockConstraints _ = []

data UserLockedEscrow

instance Escrow UserLockedEscrow where
    data EscrowUnlock UserLockedEscrow
    unlockConstraints _ = [TxSigned "TODO_USER_PARAM"]

data TokenLockedEscrow

{-

instance Escrow TokenLockedEscrow where
    data EscrowUnlock TokenLockedEscrow = NoData
    unlockConstraints _ = [TxInC kind "TODO_TOKEN_PARAM"]
        where
            kind = InRef


-}

data HashLockedEscrow

{-

instance Escrow HashLockedEscrow where
    data EscrowUnlock UnboundedEscrow = NoData
    unlockConstraints _ = []

-}

data SwapEscrow

{-
instance Escrow SwapEscrow where
    data EscrowUnlock SwapEscrow = NoData
    unlockConstraints _ = [
        TxInC In "TODO_TOKEN_PARAM"
    ]
-}
