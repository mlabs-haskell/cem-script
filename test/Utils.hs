module Utils where

import Prelude

import Data.Map (keys)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V1.Value (assetClassValue)

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
 )

import Test.Hspec (shouldSatisfy)
import Text.Show.Pretty (ppShow)

import Cardano.CEM.Monads (
  BlockchainMonadEvent (..),
  CEMAction (..),
  Fees (..),
  MonadBlockchainParams (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  ResolvedTx (..),
  SomeCEMAction (..),
  TxSpec (..),
  UtxoQuery (..),
  submitResolvedTx,
 )
import Cardano.CEM.Monads.CLB (ClbRunner, execOnIsolatedClb)
import Cardano.CEM.OffChain (
  awaitTx,
  fromPlutusAddressInMonad,
  resolveTxAndSubmit,
 )
import Cardano.Extras
import Data.Spine (HasSpine (..))

import TestNFT

execClb :: ClbRunner a -> IO a
execClb = execOnIsolatedClb $ lovelaceToValue $ fromInteger 300_000_000

mintTestTokens ::
  (MonadIO m, MonadSubmitTx m) => SigningKey PaymentKey -> Integer -> m ()
mintTestTokens userSk numMint = do
  userAddress <- fromPlutusAddressInMonad $ signingKeyToAddress userSk
  utxo <- queryUtxo $ ByAddresses [signingKeyToAddress userSk]

  let
    user1TxIns = keys $ unUTxO utxo
    convert x =
      TxOutValueShelleyBased shelleyBasedEra $
        toMaryValue x
    out =
      TxOut
        userAddress
        ( convert $
            ( fromPlutusValue $
                assetClassValue
                  testNftAssetClass
                  numMint
            )
              <> (lovelaceToValue $ fromInteger 3_000_000)
        )
        TxOutDatumNone
        ReferenceScriptNone
    tx =
      MkResolvedTx
        { txIns = map withKeyWitness user1TxIns
        , txInRefs = []
        , txOuts = [out]
        , toMint =
            mintedTokens
              (PlutusScriptSerialised testNftPolicy)
              ()
              [(tokenToAsset testNftTokenName, fromInteger numMint)]
        , interval = always
        , additionalSigners = []
        , signer = userSk
        }
  awaitEitherTx =<< submitResolvedTx tx
  return ()

checkTxCreated ::
  (MonadQueryUtxo m, MonadIO m) => TxId -> m ()
checkTxCreated txId = do
  -- FIXME: better checks for tests
  awaitTx txId
  let
    txIn = TxIn txId (TxIx 0)
    someValue = lovelaceToValue 0
  utxo <- queryUtxo $ ByTxIns [txIn]
  liftIO $ shouldSatisfy (utxoValue utxo) (/= someValue)

awaitEitherTx ::
  (MonadQueryUtxo m, MonadIO m, Show error) => Either error TxId -> m ()
awaitEitherTx eitherTx =
  case eitherTx of
    Right txId -> do
      awaitTx txId
    Left errorMsg -> error $ "Failed to send tx: " <> ppShow errorMsg

submitAndCheck :: (MonadSubmitTx m, MonadIO m) => TxSpec -> m ()
submitAndCheck spec = do
  case head $ actions spec of
    MkSomeCEMAction (MkCEMAction _ transition) ->
      liftIO $ putStrLn $ "Doing " <> show transition
  awaitEitherTx =<< resolveTxAndSubmit spec

perTransitionStats :: (MonadBlockchainParams m) => m (Map.Map String Fees)
perTransitionStats = do
  events <- eventList
  let feesByTxId = Map.fromList $ mapMaybe txIdFeePair events
  return $ Map.fromList $ mapMaybe (transitionFeePair feesByTxId) events
  where
    txIdFeePair (UserSpentFee {fees, txId}) = Just (txId, fees)
    txIdFeePair _ = Nothing
    transitionFeePair feesByTxId event = case event of
      ( SubmittedTxSpec
          (MkTxSpec [MkSomeCEMAction (MkCEMAction _ transition)] _)
          (Right txId)
        ) ->
          Just (show (getSpine transition), feesByTxId Map.! txId)
      _ -> Nothing
