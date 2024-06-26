{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CEM.Monads.CLB where

import Prelude

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.State (StateT (..), gets)
import Data.Map qualified as Map
import Data.Set qualified as Set

-- Cardano imports
import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Query (fromLedgerUTxO)

-- Lib imports
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

-- CEM imports

import Cardano.CEM.Monads
import Cardano.CEM.Monads.L1Commons
import Cardano.CEM.OffChain (fromPlutusAddressInMonad)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))

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
    eraHistory <- LedgerEpochInfo <$> getEpochInfo
    let systemStart =
          SystemStart $ posixTimeToUTCTime $ scSlotZeroTime slotConfig
    return $
      MkBlockchainParams
        { protocolParameters
        , systemStart
        , eraHistory
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
    utxos <- fromLedgerUTxO shelleyBasedEra <$> gets getUtxosAtState
    predicate <- mkPredicate
    return $ UTxO $ Map.filterWithKey predicate $ unUTxO utxos
    where
      mkPredicate = case query of
        ByAddresses addresses -> do
          cardanoAddresses <- mapM fromPlutusAddressInMonad addresses
          return $ \_ (TxOut a _ _ _) -> a `elem` cardanoAddresses
        ByTxIns txIns -> return $ \txIn _ -> txIn `elem` txIns

instance (Monad m, MonadBlockchainParams (ClbT m)) => MonadSubmitTx (ClbT m) where
  submitResolvedTx :: ResolvedTx -> ClbT m (Either TxSubmittingError TxId)
  submitResolvedTx tx = do
    cardanoTxBodyFromResolvedTx tx >>= \case
      Right (body, TxInMode ShelleyBasedEraBabbage tx') -> do
        result <- sendTx tx'
        case result of
          Success _ _ -> return $ Right $ getTxId body
          Fail _ validationError ->
            return $ Left $ UnhandledNodeSubmissionError validationError
      Right (_, _) -> fail "Unsupported tx format"
      Left e -> return $ Left $ UnhandledAutobalanceError e

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
