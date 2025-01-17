{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CEM.Monads.CLB where

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Query (fromLedgerUTxO)
import Cardano.CEM.Monads
import Cardano.CEM.Monads.L1Commons
import Cardano.CEM.OffChain (fromPlutusAddressInMonad)
import Cardano.Extras (Era)
import Clb (
  ClbState (mockConfig),
  ClbT (..),
  MockConfig (..),
  SlotConfig (scSlotZeroTime),
  ValidationResult (..),
  getCurrentSlot,
  getEpochInfo,
  getUtxosAtState,
  initClb,
  intToCardanoSk,
  sendTx,
 )
import Clb.MockConfig (defaultBabbage)
import Clb.TimeSlot (posixTimeToUTCTime)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (StateT (..), gets)
import Data.Either.Extra (mapRight)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Prelude

instance (MonadReader r m) => MonadReader r (ClbT m) where
  ask = lift ask
  local f action = ClbT $ local f $ unwrapClbT action

type ClbRunner = ClbT (ReaderT (MVar [BlockchainMonadEvent]) IO)

instance
  ( MonadFail m
  , MonadIO m
  , MonadReader (MVar [BlockchainMonadEvent]) m
  ) =>
  MonadBlockchainParams (ClbT m)
  where
  askNetworkId = gets (mockConfigNetworkId . mockConfig)

  queryCurrentSlot = getCurrentSlot

  queryBlockchainParams = do
    protocolParameters <- gets (mockConfigProtocol . mockConfig)
    slotConfig <- gets (mockConfigSlotConfig . mockConfig)
    ledgerEpochInfo <- LedgerEpochInfo <$> getEpochInfo
    let systemStart =
          SystemStart $ posixTimeToUTCTime $ scSlotZeroTime slotConfig
    return $
      MkBlockchainParams
        { protocolParameters
        , systemStart
        , ledgerEpochInfo
        , -- Staking is not supported
          stakePools = Set.empty
        }
  logEvent e = do
    logVar <- ask
    liftIO $ modifyMVar_ logVar (return . (:) e)
  eventList = do
    events <- ask
    liftIO $ readMVar events

instance (Monad m, MonadBlockchainParams (ClbT m)) => MonadQueryUtxo (ClbT m) where
  queryUtxo query = do
    utxos <- gets (fromLedgerUTxO shelleyBasedEra . getUtxosAtState)
    predicate <- mkPredicate
    return $ UTxO $ Map.filterWithKey predicate $ unUTxO utxos
    where
      mkPredicate = case query of
        ByAddresses addresses -> do
          cardanoAddresses <- mapM fromPlutusAddressInMonad addresses
          return $ \_ (TxOut a _ _ _) -> a `elem` cardanoAddresses
        ByTxIns txIns -> return $ \txIn _ -> txIn `elem` txIns

instance (Monad m, MonadBlockchainParams (ClbT m)) => MonadSubmitTx (ClbT m) where
  submitResolvedTxRet ::
    ResolvedTx ->
    ClbT m (Either TxSubmittingError (TxBodyContent BuildTx Era, TxBody Era, TxInMode, UTxO Era))
  submitResolvedTxRet tx = do
    cardanoTxBodyFromResolvedTx tx >>= \case
      Right (preBody, body, txInMode@(TxInMode ShelleyBasedEraBabbage tx'), utxo) -> do
        result <- sendTx tx'
        case result of
          Success _ _ -> return $ Right (preBody, body, txInMode, utxo)
          Fail _ validationError ->
            return $ Left $ UnhandledNodeSubmissionError validationError
      Right _ -> fail "Unsupported tx format"
      Left e -> return $ Left $ UnhandledAutobalanceError e

  submitResolvedTx :: ResolvedTx -> ClbT m (Either TxSubmittingError TxId)
  submitResolvedTx tx = mapRight (getTxId . (\(_, a, _, _) -> a)) <$> submitResolvedTxRet tx

instance (Monad m, MonadBlockchainParams (ClbT m)) => MonadTest (ClbT m) where
  getTestWalletSks = return $ map intToCardanoSk [1 .. 10]

genesisClbState :: Value -> ClbState
genesisClbState genesisValue =
  initClb defaultBabbage genesisValue genesisValue

execOnIsolatedClb :: Value -> ClbRunner a -> IO a
execOnIsolatedClb genesisValue action = do
  emptyLog <- newMVar []
  fst
    <$> runReaderT
      ( runStateT
          (unwrapClbT action)
          (genesisClbState genesisValue)
      )
      emptyLog
