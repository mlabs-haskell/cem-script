{-# LANGUAGE RecordWildCards #-}

-- | Code common for resolving Tx of backends which use `cardano-api`
module Cardano.CEM.Monads.L1Commons where

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (LedgerProtocolParameters (..))
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.Extras
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Prelude

-- | Main function
cardanoTxBodyFromResolvedTx ::
  (MonadQueryUtxo m, MonadBlockchainParams m) =>
  ResolvedTx ->
  m (Either (TxBodyErrorAutoBalance Era) (TxBodyContent BuildTx Era, TxBody Era, TxInMode, UTxO Era))
cardanoTxBodyFromResolvedTx (MkResolvedTx {..}) = do
  utxo <- queryUtxo $ ByAddresses [signingKeyToAddress signer]
  let
    feeTxIns = Map.keys $ unUTxO utxo
    allTxIns = txIns ++ map withKeyWitness feeTxIns

  signerAddress <- fromPlutusAddressInMonad $ signingKeyToAddress signer
  MkBlockchainParams {protocolParameters} <- queryBlockchainParams

  let additionalSignersKeys =
        filter (\x -> signingKeyToPKH x `elem` additionalSigners) [signer]

  let preBody =
        TxBodyContent
          { txIns = nub allTxIns -- duplicate TxIn for coin-selection redeemer bug
          , txInsCollateral =
              TxInsCollateral AlonzoEraOnwardsBabbage feeTxIns
          , txInsReference =
              TxInsReference BabbageEraOnwardsBabbage txInRefs
          , txOuts
          , txMintValue = toMint
          , -- Adding all keys here, cuz other way `txSignedBy` does not see those
            -- signatures
            txExtraKeyWits =
              TxExtraKeyWitnesses AlonzoEraOnwardsBabbage $
                verificationKeyHash . getVerificationKey
                  <$> additionalSignersKeys
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
          , -- Not supported features
            txMetadata = TxMetadataNone
          , txAuxScripts = TxAuxScriptsNone
          , txWithdrawals = TxWithdrawalsNone
          , txCertificates = TxCertificatesNone
          , txUpdateProposal = TxUpdateProposalNone
          , txScriptValidity = TxScriptValidityNone
          , txProposalProcedures = Nothing
          , txVotingProcedures = Nothing
          }

  txInsUtxo <- queryUtxo $ ByTxIns $ map fst allTxIns

  runExceptT $ do
    body <-
      ExceptT $
        callBodyAutoBalance
          preBody
          txInsUtxo
          signerAddress
    let
      tx = makeSignedTransactionWithKeys [signer] body
      txInMode = TxInMode ShelleyBasedEraBabbage tx

    lift $ recordFee txInsUtxo body

    return (preBody, body, txInMode, txInsUtxo)
  where
    recordFee txInsUtxo body@(TxBody content) = do
      case txFee content of
        TxFeeExplicit era coin -> do
          MkBlockchainParams {protocolParameters, systemStart, ledgerEpochInfo} <-
            queryBlockchainParams
          Right report <-
            return $
              evaluateTransactionExecutionUnits
                (shelleyBasedToCardanoEra era)
                systemStart
                ledgerEpochInfo
                (LedgerProtocolParameters protocolParameters)
                txInsUtxo
                body
          let
            rights = mapMaybe $ \case
              Right x -> Just x
              Left _ -> Nothing
            budgets = rights $ map snd $ Map.toList report
            usedMemory = sum $ executionMemory <$> budgets
            usedCpu = sum $ executionSteps <$> budgets
          logEvent $
            UserSpentFee
              { fees =
                  MkFees
                    { fee = coin
                    , usedMemory
                    , usedCpu
                    }
              , txId = getTxId body
              , txSigner = signer
              }

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
  m (Either (TxBodyErrorAutoBalance Era) (TxBody Era))
callBodyAutoBalance
  preBody
  utxo
  changeAddress = do
    MkBlockchainParams {protocolParameters, systemStart, ledgerEpochInfo, stakePools} <-
      queryBlockchainParams
    let result =
          makeTransactionBodyAutoBalance @Era
            shelleyBasedEra
            systemStart
            ledgerEpochInfo
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
