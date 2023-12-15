module Cardano.CEM.Monads where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Set (Set)

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  UnsafeFromData (..),
  always,
  fromData,
 )

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns)
import Cardano.Api.Shelley (PlutusScript (..), PoolId, ReferenceScript (..), fromPlutusData, toMaryValue, toPlutusData)
import Cardano.Ledger.Core (PParams)

import Cardano.Api.IPC (TxValidationError)
import Cardano.CEM
import Cardano.CEM.OnChain
import Cardano.Extras
import Cardano.Ledger.Shelley.API (ApplyTxError)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.Trans (MonadTrans (..))
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Spine (HasSpine (..))
import Text.Show.Pretty (ppShow)

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
  , signer :: [SigningKey PaymentKey]
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
