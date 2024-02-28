module Cardano.CEM.Monads where

import Prelude

import PlutusLedgerApi.V1.Address (Address)

import Cardano.Api hiding (Address)

import Cardano.CEM

-- Common

type Era = BabbageEra

-- MonadBlockchainParams

{- | This monad gives access to all information about Cardano params,
 | which is various kind of Ledger params and ValidityBound/Slots semantics
-}

-- @todo #13: Add rest of MonadBlockchainParams interface
-- @todo #13: Implement PSM for MonadBlockchainParams
class (Monad m) => MonadBlockchainParams m where
  askNetworkId :: m NetworkId
  queryCurrentSlot :: m SlotNo

-- @todo #12: copy `fromPlutusAddressInMonad` from Hydra

-- MonadQuery

data UtxoQuery
  = ByAddress Address
  | ByTxIns [TxIn]

class (MonadBlockchainParams m) => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m (UTxO Era)

-- MonadQuery

-- @todo #12: specify ResolvedTx and TxResolutionStrategy
data ResolvedTx

data WrongSlotKind = Early | Late

data TxSubmittingError
  = WrongSlot WrongSlotKind Integer
  | TxInOutdated [TxIn]
  | UnhandledNodeError String

class (MonadQueryUtxo m) => MonadSubmitTx m where
  submitResolvedTx :: ResolvedTx -> m (Either TxSubmittingError TxId)

data TxResolutionStrategy

data TxSigner = MkTxSigner
  { signerAddress :: Address
  , allowTxInSpending :: Bool
  , allowFeeCovering :: Bool
  }

data SomeTransitionSpec where
  MkSomeTransitionSpec ::
    forall spec. (CEMScript spec) => TransitionSpec spec -> SomeTransitionSpec

data TxSpec = MkTxSpec
  { transitionSpec :: [SomeTransitionSpec]
  , signers :: [TxSigner]
  , changeAddress :: Address
  }

data TxResolutionError
  = TxSpecIsIncorrect
  | UnhandledSubmittingError TxSubmittingError

resolveTxAndSubmit ::
  TxSpec -> TxResolutionStrategy -> Either TxResolutionError TxId
resolveTxAndSubmit = error "TODO"
