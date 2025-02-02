module Cardano.CEM.Monads where

import Prelude

import Data.Set (Set)
import GHC.Natural (Natural)

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  PubKeyHash,
 )

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns)
import Cardano.Api.Shelley (PoolId)
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Shelley.API (ApplyTxError (..), Coin)

import Cardano.CEM.DSL
import Cardano.CEM.OnChain
import Cardano.Extras

-- CEMAction and TxSpec

data CEMAction script = MkCEMAction (Params script) (Transition script)

deriving stock instance (CEMScript script) => Show (CEMAction script)

-- TODO: can we rmove this by adding existential type to CEMAction?
data SomeCEMAction where
  MkSomeCEMAction ::
    forall script.
    (CEMScriptCompiled script) =>
    CEMAction script ->
    SomeCEMAction

instance Show SomeCEMAction where
  show :: SomeCEMAction -> String
  show (MkSomeCEMAction action) = show action

{- | Transaction specification reflects the fact that a single transaction
can involve several scripts by having an action for every script.
-}
data TxSpec = MkTxSpec
  { actions :: [SomeCEMAction]
  , specSigner :: SigningKey PaymentKey
  }
  deriving stock (Show)

-- MonadBlockchainParams

-- | Params of blockchain required for transaction-building
data BlockchainParams = MkBlockchainParams
  { protocolParameters :: PParams LedgerEra
  , systemStart :: SystemStart
  , ledgerEpochInfo :: LedgerEpochInfo
  , stakePools :: Set PoolId
  }
  deriving stock (Show)

data Fees = MkFees
  { fee :: Coin
  , usedMemory :: Natural
  , usedCpu :: Natural
  }
  deriving stock (Show)

data BlockchainMonadEvent
  = SubmittedTxSpec TxSpec (Either () TxId) -- (Either TxResolutionError TxId)
  | UserSpentFee
      { txId :: TxId
      , txSigner :: SigningKey PaymentKey
      , fees :: Fees
      }
  | AwaitedTx TxId
  deriving stock (Show)

{- | This monad gives access to all information about Cardano params,
 which is various kind of Ledger params and ValidityBound/Slots semantics

 Also contains common structured log support.
-}
class (MonadFail m) => MonadBlockchainParams m where
  askNetworkId :: m NetworkId
  queryCurrentSlot :: m SlotNo
  queryBlockchainParams :: m BlockchainParams
  logEvent :: BlockchainMonadEvent -> m ()
  eventList :: m [BlockchainMonadEvent]

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
  , txInRefs :: [TxIn]
  , txOuts :: [TxOut CtxTx Era]
  , toMint :: TxMintValue BuildTx Era
  , interval :: Interval POSIXTime
  , additionalSigners :: [PubKeyHash]
  , signer :: ~(SigningKey PaymentKey)
  }
  deriving stock (Show, Eq)

data WrongSlotKind = Early | Late
  deriving stock (Show, Eq)

data TxSubmittingError
  = WrongSlot WrongSlotKind Integer
  | TxInOutdated [TxIn]
  | UnhandledAutobalanceError (TxBodyErrorAutoBalance Era)
  | UnhandledNodeSubmissionError (ApplyTxError LedgerEra)
  deriving stock (Show)

-- | Error occurred while trying to execute CEMScript transition
data TransitionError
  = CannotFindTransitionInput String
  | CompilationError String
  | SpecExecutionError {errorMessage :: String}
  deriving stock (Show, Eq)

-- | Ability to send transaction to chain
class (MonadQueryUtxo m) => MonadSubmitTx m where
  submitResolvedTx :: ResolvedTx -> m (Either TxSubmittingError TxId)
  submitResolvedTxRet ::
    ResolvedTx ->
    m (Either TxSubmittingError (TxBodyContent BuildTx Era, TxBody Era, TxInMode, UTxO Era))

-- | Stuff needed to use monad for local testing
class (MonadSubmitTx m) => MonadTest m where
  -- | List of keys having some amount of genesis ADA
  getTestWalletSks :: m [SigningKey PaymentKey]
