module Utils where

import Prelude

import Control.Monad.Trans (MonadIO (..))
import Data.Map (elems, keys)

import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V1.Value (adaSymbol, adaToken, assetClass, assetClassValue)

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
 )

import Test.Hspec (shouldSatisfy)
import Text.Show.Pretty (ppShow)

import Cardano.CEM.Monads (
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  ResolvedTx (..),
  UtxoQuery (..),
 )
import Cardano.CEM.Monads.CLB (execOnIsolatedClb)
import Cardano.CEM.OffChain (
  CEMAction (..),
  SomeCEMAction (..),
  TxSpec (..),
  awaitTx,
  fromPlutusAddressInMonad,
  resolveTxAndSubmit,
 )
import Cardano.Extras

import TestNFT

execClb = execOnIsolatedClb $ lovelaceToValue $ fromInteger 300_000_000

mintTestTokens userSk numMint = do
  userAddress <- fromPlutusAddressInMonad $ signingKeyToAddress userSk
  utxo <- queryUtxo $ ByAddresses [signingKeyToAddress userSk]

  let
    user1TxIns = keys $ unUTxO utxo
    Just value = valueToLovelace $ utxoValue utxo
    convert x =
      TxOutValueShelleyBased shelleyBasedEra $
        toMaryValue x
    out userAddress =
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
        , txInsReference = []
        , txOuts =
            [ out userAddress
            ]
        , toMint =
            mintedTokens
              (PlutusScriptSerialised testNftPolicy)
              ()
              [(tokenToAsset testNftTokenName, fromInteger numMint)]
        , interval = always
        , signer = [userSk]
        }
  awaitEitherTx =<< submitResolvedTx tx
  return ()

checkTxCreated ::
  (MonadQueryUtxo m, MonadIO m) => TxId -> m ()
checkTxCreated txId = do
  -- TODO: better out checks
  awaitTx txId
  let
    txIn = TxIn txId (TxIx 0)
    someValue = lovelaceToValue $ fromInteger 0
  utxo <- queryUtxo $ ByTxIns [txIn]
  liftIO $ shouldSatisfy (utxoValue utxo) (/= someValue)

awaitEitherTx ::
  (MonadQueryUtxo m, MonadIO m, Show error) => Either error TxId -> m ()
awaitEitherTx eitherTx =
  case eitherTx of
    Right txId -> do
      awaitTx txId
    -- liftIO $ putStrLn $ "Awaited " <> show txId
    Left errorMsg -> error $ "Failed to send tx: " <> ppShow errorMsg

submitAndCheck spec = do
  case head $ actions spec of
    MkSomeCEMAction (MkCEMAction _ transition) ->
      liftIO $ putStrLn $ "Doing " <> show transition
  awaitEitherTx =<< resolveTxAndSubmit spec
