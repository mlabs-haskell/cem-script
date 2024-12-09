{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OuraFilters.Simple (simpleSpec) where

import Cardano.CEM.Indexing.Oura qualified as Config
import Cardano.CEM.Indexing.Tx qualified as Tx
import Control.Lens (ix, (.~))
import Control.Monad ((>=>))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text qualified as T
import Oura.Communication (Oura (receive, send, shutDown))
import Oura.Communication qualified as Oura
import OuraFilters.Auction qualified
import OuraFilters.Mock qualified as Mock
import PlutusLedgerApi.V1 qualified as V1
import Test.Hspec (Spec, focus, it, shouldBe)
import Utils qualified
import Prelude

exampleMatchingTx :: Mock.TxEvent
exampleMatchingTx =
  exampleTx
    & Mock.parsed_tx . Tx.inputs . ix 0 . Tx.as_output . Tx.address .~ inputAddress
  where
    inputAddress = Tx.MkAddressAsBase64 "AZSTMVzZLrXYxDBOZ7fhauNtYdNFAmlGV4EaLI4ze2LP/2QDoGo6y8NPjEYAPGn+eaNijO+pxHJR"

exampleFilter :: Config.Filter
exampleFilter = Config.selectByAddress "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"

exampleTx :: Mock.TxEvent
exampleTx =
  Mock.mkTxEvent $
    Tx.arbitraryTx
      & Tx.inputs
        .~ [ Tx.MkTxInput
              { Tx._tx_hash = Tx.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
              , Tx._output_index = 5
              , Tx._as_output = out
              , Tx._redeemer =
                  Just $
                    Tx.MkRedeemer
                      { _purpose = Tx.PURPOSE_UNSPECIFIED
                      , payload = Tx.encodePlutusData (V1.I 212)
                      }
              }
           ]
      & Tx.outputs .~ [out]
      & Tx.txCollateral . Tx.collateral_return . Tx.coin .~ 25464
      & Tx.txCollateral . Tx.total_collateral .~ 2555
      & Tx.fee .~ 967
      & Tx.validity .~ Tx.MkTxValidity {Tx._start = 324, Tx._ttl = 323}
  where
    out =
      Tx.MkTxOutput
        { Tx._address = Tx.MkAddressAsBase64 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
        , Tx._coin = 254564
        , Tx._assets = []
        , Tx._datum =
            Just $
              Tx.MkDatum
                { Tx._payload =
                    Tx.encodePlutusData $
                      V1.List
                        [ V1.Map
                            [ (V1.I 2, V1.I 33)
                            ]
                        , V1.Constr 3 [V1.I 288]
                        , V1.I 34
                        , V1.B "aboba"
                        ]
                , Tx.hash = Tx.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
                , Tx._original_cbor = ""
                }
        , Tx._script = Nothing
        }

simpleSpec :: Spec
simpleSpec = Utils.killProcessesOnError do
  focus $ it "Oura filters match tx it have to match, and don't match other" \spotGarbage ->
    let
      tx = Mock.txToBS exampleTx
      matchingTx = Mock.txToBS exampleMatchingTx
     in
      Oura.withOura
        (Oura.MkWorkDir "./tmp")
        spotGarbage
        (Config.daemonConfig [exampleFilter])
        \oura -> do
          Utils.withTimeout 3.0 do
            oura.send tx
            oura.send matchingTx
            Right outTxHash <- do
              bs <- oura.receive
              print bs
              pure $ extractOutputTxHash bs
            Right inputTxHash <-
              pure $ extractInputTxHash matchingTx
            outTxHash `shouldBe` inputTxHash
            oura.shutDown
  OuraFilters.Auction.spec

extractInputTxHash :: BS.ByteString -> Either String T.Text
extractInputTxHash =
  Aeson.eitherDecodeStrict >=> Aeson.parseEither \json -> do
    parsedTx <- json .: "parsed_tx"
    parsedTx .: "hash"

extractOutputTxHash :: BS.ByteString -> Either String T.Text
extractOutputTxHash =
  Aeson.eitherDecodeStrict >=> Aeson.parseEither \json -> do
    parsedTx <- json .: "record"
    parsedTx .: "hash"
