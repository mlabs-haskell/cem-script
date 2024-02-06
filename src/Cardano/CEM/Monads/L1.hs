{-# LANGUAGE RecordWildCards #-}

module Cardano.CEM.Monads.L1 where

import Prelude

import Text.Show.Pretty

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadIO (..))
import Data.Set qualified as Set
import Unsafe.Coerce (unsafeCoerce)

import Data.Aeson

-- Cardano imports
-- import Cardano.Ledger.Chain (PredicateFailure)
-- import Cardano.Ledger.Shelley.API ()
-- import Ouroboros.Consensus.Shelley.Ledger (ApplyTxError (..))
-- import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO, TranslationError)
import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (LedgerProtocolParameters (..))

-- CEM imports

import Cardano.CEM
import Cardano.CEM.Monads
import Cardano.Extras
import Control.Exception (throwIO)
import Data.Bifunctor (Bifunctor (..))
import Data.Map qualified as Map

data ExecutionContext = MkExecutionContext
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
      <$> queryCardanoNode (convertEra QueryProtocolParameters)
      <*> queryCardanoNode (convertEra QuerySystemStart)
      <*> queryCardanoNode (convertEra QueryEraHistory)
      <*> queryCardanoNode (convertEra QueryStakePools)
    where
      -- TODO
      convertEra = unsafeCoerce

-- TODO: cardano-api-extras
-- Design inspired by `Hydra.Chain.CardanoClient` helpers
queryCardanoNode ::
  QueryInShelleyBasedEra Era b -> L1Runner b
queryCardanoNode query = do
  node <- localNode <$> ask
  result <- liftIO $ queryNodeLocalState node Nothing cardanoQuery
  return $ case result of
    -- TODO: better handling of wrong-era exceptions
    Right (Right x) -> x
    _ -> error "Unhandled Cardano API error"
  where
    cardanoQuery =
      QueryInEra $ QueryInShelleyBasedEra ShelleyBasedEraBabbage query

instance MonadQueryUtxo L1Runner where
  queryUtxo query = do
    utxoQuery <- case query of
      ByTxIns txIns ->
        return $ QueryUTxOByTxIn (Set.fromList txIns)
      ByAddresses addresses -> do
        cardanoAdresses <-
          map addressInEraToAny <$> mapM fromPlutusAddressInMonad addresses
        return $ QueryUTxOByAddress (Set.fromList cardanoAdresses)
    queryCardanoNode $ QueryUTxO utxoQuery

instance MonadSubmitTx L1Runner where
  submitResolvedTx :: ResolvedTx -> L1Runner (Either TxSubmittingError TxId)
  submitResolvedTx MkResolvedTx {..} = do
    -- (lowerBound, upperBound) <- convertValidityBound validityBound
    -- TODO
    let keyWitnessedTxIns = [fst $ last txIns]
    MkBlockchainParams {protocolParameters} <- queryBlockchainParams
    let preBody =
          TxBodyContent
            { txIns = txIns
            , txInsCollateral =
                TxInsCollateral AlonzoEraOnwardsBabbage keyWitnessedTxIns
            , txInsReference =
                TxInsReference BabbageEraOnwardsBabbage txInsReference
            , txOuts
            , txMintValue = toMint
            , txExtraKeyWits =
                TxExtraKeyWitnesses AlonzoEraOnwardsBabbage $
                  fmap (verificationKeyHash . getVerificationKey) signors
            , txProtocolParams =
                BuildTxWith $
                  Just $
                    LedgerProtocolParameters protocolParameters
            , txValidityLowerBound =
                TxValidityNoLowerBound
            , txValidityUpperBound =
                TxValidityUpperBound ShelleyBasedEraBabbage Nothing
            , -- Fee stubs
              txTotalCollateral = TxTotalCollateralNone
            , txReturnCollateral = TxReturnCollateralNone
            , txFee = TxFeeExplicit ShelleyBasedEraBabbage 0
            , -- Not supported fatures
              txMetadata = TxMetadataNone
            , txAuxScripts = TxAuxScriptsNone
            , txWithdrawals = TxWithdrawalsNone
            , txCertificates = TxCertificatesNone
            , txUpdateProposal = TxUpdateProposalNone
            , txScriptValidity = TxScriptValidityNone
            , txProposalProcedures = Nothing
            , txVotingProcedures = Nothing
            }

    let
      mainSignor = signors !! 0
      mainAddress' = signingKeyToAddress mainSignor

    mainAddress <- fromPlutusAddressInMonad mainAddress'
    utxo <- queryUtxo $ ByTxIns $ map fst txIns

    -- liftIO $ pPrint preBody
    -- liftIO $ pPrint utxo

    body <-
      either (\x -> fail $ "Autobalance error: " <> show x) return
        =<< callBodyAutoBalance
          preBody
          utxo
          mainAddress

    let
      tx = makeSignedTransactionWithKeys signors body
      txInMode = TxInMode ShelleyBasedEraBabbage tx

    -- liftIO $ pPrint tx
    ci <- localNode <$> ask
    liftIO $
      submitTxToNodeLocal ci txInMode >>= \case
        SubmitSuccess ->
          return $ Right $ getTxId body
        SubmitFail e ->
          return $ Left $ UnhandledNodeError $ show e
          -- case parseError e of
          --   ApplyTxError x ->
          --    return $ Left $ UnhandledNodeError $ show x
    where

-- parseError wrapper = case wrapper of
--   TxValidationErrorInCardanoMode error ->
--       fromJSON (toJSON error .: "error") :: ApplyTxError _

-- Utils

makeSignedTransactionWithKeys ::
  [SigningKey PaymentKey] ->
  TxBody Era ->
  Tx Era
makeSignedTransactionWithKeys keys txBody =
  makeSignedTransaction keyWitnesses txBody
  where
    createWitness key = makeShelleyKeyWitness shelleyBasedEra txBody (WitnessPaymentKey key)
    keyWitnesses = fmap createWitness keys

callBodyAutoBalance ::
  (MonadBlockchainParams m) =>
  TxBodyContent BuildTx Era ->
  UTxO Era ->
  AddressInEra Era ->
  m (Either TxBodyErrorAutoBalance (TxBody Era))
callBodyAutoBalance
  preBody
  utxo
  changeAddress = do
    MkBlockchainParams {protocolParameters, systemStart, eraHistory, stakePools} <-
      queryBlockchainParams
    let result =
          makeTransactionBodyAutoBalance @Era
            shelleyBasedEra
            systemStart
            (toLedgerEpochInfo eraHistory)
            (LedgerProtocolParameters protocolParameters)
            stakePools
            Map.empty -- Stake credentials
            Map.empty -- Some other DRep stuff
            utxo
            preBody
            changeAddress
            Nothing
    return $ fmap balancedTxBody result
    where
      balancedTxBody (BalancedTxBody _ txBody _ _) = txBody

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
