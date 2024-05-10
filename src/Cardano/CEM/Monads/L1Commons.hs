{-# LANGUAGE RecordWildCards #-}

-- | Code common for resolving Tx of backends which use `cardano-api`
module Cardano.CEM.Monads.L1Commons where

import Prelude

import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Map qualified as Map

-- Cardano imports
import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (LedgerProtocolParameters (..))

-- Project imports
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.Extras

-- Main function

cardanoTxBodyFromResolvedTx ::
  (MonadQueryUtxo m, MonadBlockchainParams m) =>
  ResolvedTx ->
  m (Either (TxBodyErrorAutoBalance Era) (TxBody Era, TxInMode))
cardanoTxBodyFromResolvedTx (MkResolvedTx {..}) = do
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
              -- Somehow now it does not requires them, while before does
              TxExtraKeyWitnesses AlonzoEraOnwardsBabbage []
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

  let
    mainSignor = signer !! 0
    mainAddress' = signingKeyToAddress mainSignor

  mainAddress <- fromPlutusAddressInMonad mainAddress'
  utxo <- queryUtxo $ ByTxIns $ map fst txIns

  runExceptT $ do
    body <-
      ExceptT $
        callBodyAutoBalance
          preBody
          utxo
          mainAddress
    let
      tx = makeSignedTransactionWithKeys signer body
      txInMode = TxInMode ShelleyBasedEraBabbage tx
    return (body, txInMode)

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
    MkBlockchainParams {protocolParameters, systemStart, eraHistory, stakePools} <-
      queryBlockchainParams
    let result =
          makeTransactionBodyAutoBalance @Era
            shelleyBasedEra
            systemStart
            eraHistory
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
