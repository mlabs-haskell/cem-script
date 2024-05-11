module Cardano.CEM.Monads where

import Prelude

import Data.Set (Set)

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  PubKeyHash,
 )

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns)
import Cardano.Api.IPC (TxValidationError)
import Cardano.Api.Shelley (PoolId)
import Cardano.Ledger.Core (PParams)

import Cardano.Extras

-- MonadBlockchainParams

-- | Params of blockchain required for transaction-building
data BlockchainParams = MkBlockchainParams
  { protocolParameters :: PParams LedgerEra
  , systemStart :: SystemStart
  , -- FIXME: rename
    eraHistory :: LedgerEpochInfo
  , stakePools :: Set PoolId
  }
  deriving stock (Show)

{- | This monad gives access to all information about Cardano params,
 | which is various kind of Ledger params and ValidityBound/Slots semantics
-}
class (MonadFail m) => MonadBlockchainParams m where
  askNetworkId :: m NetworkId
  queryCurrentSlot :: m SlotNo
  queryBlockchainParams :: m BlockchainParams

-- MonadQuery

data UtxoQuery
  = ByAddresses [Address]
  | ByTxIns [TxIn]
  deriving stock (Show, Eq)

-- | Ability to query current Utxo state of chain
class (MonadBlockchainParams m) => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m (UTxO Era)

-- MonadSubmit

data ResolvedTx = MkResolvedTx
  { txIns :: [(TxIn, TxInWitness)]
  , txInsReference :: [TxIn]
  , txOuts :: [TxOut CtxTx Era]
  , toMint :: TxMintValue BuildTx Era
  , interval :: Interval POSIXTime
  , additionalSigners :: [PubKeyHash]
  , -- FIXME: rename
    signer :: [SigningKey PaymentKey]
  }
  deriving stock (Show, Eq)

data WrongSlotKind = Early | Late
  deriving stock (Show, Eq)

data TxSubmittingError
  = WrongSlot WrongSlotKind Integer
  | TxInOutdated [TxIn]
  | UnhandledAutobalanceError (TxBodyErrorAutoBalance Era)
  | UnhandledNodeSubmissionError (TxValidationError Era)
  deriving stock (Show)

-- | Ability to send transaction to chain
class (MonadQueryUtxo m) => MonadSubmitTx m where
  submitResolvedTx :: ResolvedTx -> m (Either TxSubmittingError TxId)

-- | Stuff needed to use monad for local testing
class (MonadSubmitTx m) => MonadTest m where
  -- | List of keys having some amount of genesis ADA
  getTestWalletSks :: m [SigningKey PaymentKey]
