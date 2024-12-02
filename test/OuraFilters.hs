{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module OuraFilters (ouraFiltersSpec) where

import Cardano.CEM.Indexing qualified as Config
import Control.Lens (ix, (.~))
import Control.Monad ((>=>))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text qualified as T
import Oura (Oura (receive, send, shutDown))
import Oura qualified
import OuraFilters.Auction qualified
import OuraFilters.Mock qualified as Mock
import PlutusLedgerApi.V1 qualified as V1
import Test.Hspec (Spec, focus, it, shouldBe)
import Utils qualified
import Prelude

exampleMatchingTx :: Mock.TxEvent
exampleMatchingTx =
  exampleTx
    & Mock.parsed_tx . Mock.inputs . ix 0 . Mock.as_output . Mock.address .~ inputAddress
  where
    inputAddress = Mock.MkAddressAsBase64 "AZSTMVzZLrXYxDBOZ7fhauNtYdNFAmlGV4EaLI4ze2LP/2QDoGo6y8NPjEYAPGn+eaNijO+pxHJR"

exampleFilter :: Config.Filter
exampleFilter = Config.selectByAddress "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"

exampleTx :: Mock.TxEvent
exampleTx =
  Mock.mkTxEvent $
    Mock.arbitraryTx
      & Mock.inputs
        .~ [ Mock.MkTxInput
              { Mock._tx_hash = Mock.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
              , Mock._output_index = 5
              , Mock._as_output = out
              , Mock._redeemer =
                  Just $
                    Mock.MkRedeemer
                      { _purpose = Mock.PURPOSE_UNSPECIFIED
                      , payload = Mock.encodePlutusData (V1.I 212)
                      }
              }
           ]
      & Mock.outputs .~ [out]
      & Mock.txCollateral . Mock.collateral_return . Mock.coin .~ 25464
      & Mock.txCollateral . Mock.total_collateral .~ 2555
      & Mock.fee .~ 967
      & Mock.validity .~ Mock.MkTxValidity {Mock._start = 324, Mock._ttl = 323}
  where
    out =
      Mock.MkTxOutput
        { Mock._address = Mock.MkAddressAsBase64 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
        , Mock._coin = 254564
        , Mock._assets = []
        , Mock._datum =
            Just $
              Mock.MkDatum
                { Mock._payload =
                    Mock.encodePlutusData $
                      V1.List
                        [ V1.Map
                            [ (V1.I 2, V1.I 33)
                            ]
                        , V1.Constr 3 [V1.I 288]
                        , V1.I 34
                        , V1.B "aboba"
                        ]
                , Mock.hash = Mock.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
                , Mock._original_cbor = ""
                }
        , Mock._script = Nothing
        }

ouraFiltersSpec :: Spec
ouraFiltersSpec = Utils.killProcessesOnError do
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
