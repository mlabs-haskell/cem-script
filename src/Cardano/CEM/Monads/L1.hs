{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.CEM.Monads.L1 where

import Prelude

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Unsafe.Coerce (unsafeCoerce)

-- Cardano imports
import Cardano.Api hiding (queryUtxo)
import Cardano.Api.IPC (TxValidationError)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

-- Project imports

import Cardano.CEM.Monads (
  BlockchainParams (MkBlockchainParams),
  MonadBlockchainParams (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  MonadTest (..),
  ResolvedTx,
  TxSubmittingError (..),
  UtxoQuery (..),
 )
import Cardano.CEM.Monads.L1Commons (cardanoTxBodyFromResolvedTx)
import Cardano.CEM.OffChain (fromPlutusAddressInMonad)
import Cardano.Extras (Era, addressInEraToAny, cardanoModeParams, parseSigningKeyTE)

newtype ExecutionContext = MkExecutionContext
  { localNode :: LocalNodeConnectInfo
  }

newtype L1Runner a = MkL1Runner
  { unL1Runner :: ReaderT ExecutionContext IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader ExecutionContext
    )

-- Monad implementations

instance MonadBlockchainParams L1Runner where
  askNetworkId = localNodeNetworkId . localNode <$> ask
  queryCurrentSlot = do
    node <- localNode <$> ask
    tip <- liftIO $ getLocalChainTip node
    case tip of
      ChainTipAtGenesis -> pure 0
      ChainTip slotNo _ _ -> pure slotNo

  queryBlockchainParams = do
    MkBlockchainParams
      <$> queryCardanoNodeWrapping QueryProtocolParameters
      <*> queryCardanoNode QuerySystemStart
      <*> (toLedgerEpochInfo <$> queryCardanoNode QueryEraHistory)
      <*> queryCardanoNodeWrapping QueryStakePools

queryCardanoNodeWrapping :: QueryInShelleyBasedEra Era b -> L1Runner b
queryCardanoNodeWrapping query =
  handleEitherEra =<< queryCardanoNode (wrapQuery query)
  where
    handleEitherEra (Right x) = return x
    handleEitherEra (Left _) = fail "Unexpected era mismatch"
    wrapQuery query = QueryInEra (QueryInShelleyBasedEra shelleyBasedEra query)

-- Design inspired by `Hydra.Chain.CardanoClient` helpers
queryCardanoNode ::
  QueryInMode b -> L1Runner b
queryCardanoNode query = do
  node <- localNode <$> ask
  result <- liftIO $ queryNodeLocalState node VolatileTip query
  case result of
    -- FIXME: better handling of wrong-era exceptions
    Right x -> return x
    _ -> fail "Unhandled Cardano API error"

instance MonadQueryUtxo L1Runner where
  queryUtxo query = do
    utxoQuery <- case query of
      ByTxIns txIns ->
        return $ QueryUTxOByTxIn (Set.fromList txIns)
      ByAddresses addresses -> do
        cardanoAddresses <-
          map addressInEraToAny <$> mapM fromPlutusAddressInMonad addresses
        return $ QueryUTxOByAddress (Set.fromList cardanoAddresses)
    queryCardanoNodeWrapping $ QueryUTxO utxoQuery

instance MonadSubmitTx L1Runner where
  -- FIXME: code duplication, probably refactor out
  submitResolvedTx :: ResolvedTx -> L1Runner (Either TxSubmittingError TxId)
  submitResolvedTx tx = do
    ci <- localNode <$> ask
    cardanoTxBodyFromResolvedTx tx >>= \case
      Right (body, txInMode) ->
        liftIO $
          submitTxToNodeLocal ci txInMode >>= \case
            SubmitSuccess ->
              return $ Right $ getTxId body
            SubmitFail (TxValidationErrorInCardanoMode e) ->
              return $ Left $ UnhandledNodeSubmissionError $ unsafeCoerce e
      Left e -> return $ Left $ UnhandledAutobalanceError e

instance MonadTest L1Runner where
  -- FIXME: cache keys and better error handling
  getTestWalletSks = do
    mapM key [0 .. 2]
    where
      key n = do
        keyBytes <- liftIO $ BS.readFile $ keysPaths !! fromInteger n
        let Just key = parseSigningKeyTE keyBytes
        return key
      keysPaths =
        [ "./devnet/credentials/faucet.sk"
        , "./devnet/credentials/bob.sk"
        , "./devnet/credentials/carol.sk"
        ]

-- | Starting local devnet
localDevnetNetworkId :: NetworkId
localDevnetNetworkId = Testnet $ NetworkMagic 42

execOnLocalDevnet :: L1Runner a -> IO a
execOnLocalDevnet action =
  runReaderT (unL1Runner action) localNodeContext
  where
    localNodeContext =
      MkExecutionContext
        { localNode =
            LocalNodeConnectInfo
              cardanoModeParams
              localDevnetNetworkId
              "./devnet/node.socket"
        }
