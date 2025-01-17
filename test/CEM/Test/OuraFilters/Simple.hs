{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module CEM.Test.OuraFilters.Simple (simpleSpec) where

import CEM.Test.Oura.Communication (Oura (receive, send, shutDown))
import CEM.Test.Oura.Communication qualified as Oura
import CEM.Test.OuraFilters.Auction qualified as Auction
import CEM.Test.OuraFilters.Mock qualified as Mock
import CEM.Test.Utils
import Cardano.CEM.Indexing
import Control.Lens (ix, (.~))
import Control.Monad ((>=>))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text qualified as T
import PlutusLedgerApi.V1 qualified as V1
import Test.Hspec (Spec, focus, it, shouldBe)
import Prelude

exampleMatchingTx :: Mock.TxEvent
exampleMatchingTx =
  exampleTx
    & Mock.parsed_tx . inputs . ix 0 . as_output . address .~ inputAddress
  where
    inputAddress = MkAddressAsBase64 "AZSTMVzZLrXYxDBOZ7fhauNtYdNFAmlGV4EaLI4ze2LP/2QDoGo6y8NPjEYAPGn+eaNijO+pxHJR"

exampleFilter :: Filter
exampleFilter = selectByAddress "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"

exampleTx :: Mock.TxEvent
exampleTx =
  Mock.mkTxEvent $
    arbitraryTx
      & inputs
        .~ [ MkTxInput
              { _tx_hash = MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
              , _output_index = 5
              , _as_output = out
              , _redeemer =
                  Just $
                    MkRedeemer
                      { _purpose = PURPOSE_UNSPECIFIED
                      , payload = encodePlutusData (V1.I 212)
                      }
              }
           ]
      & outputs .~ [out]
      & txCollateral . collateral_return . coin .~ 25464
      & txCollateral . total_collateral .~ 2555
      & fee .~ 967
      & validity .~ MkTxValidity {_start = 324, _ttl = 323}
  where
    out =
      MkTxOutput
        { _address = MkAddressAsBase64 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
        , _coin = 254564
        , _assets = []
        , _datum =
            Just $
              MkDatum
                { _payload =
                    encodePlutusData $
                      V1.List
                        [ V1.Map
                            [ (V1.I 2, V1.I 33)
                            ]
                        , V1.Constr 3 [V1.I 288]
                        , V1.I 34
                        , V1.B "aboba"
                        ]
                , hash = MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
                , _original_cbor = ""
                }
        , _script = Nothing
        }

simpleSpec :: Spec
simpleSpec = killProcessesOnError do
  focus $ it "Oura filters match tx it have to match, and don't match other" \spotGarbage ->
    let
      tx = Mock.txToBS exampleTx
      matchingTx = Mock.txToBS exampleMatchingTx
     in
      Oura.withOura
        (Oura.MkWorkDir "./tmp")
        spotGarbage
        (daemonConfig [exampleFilter])
        \oura -> do
          withTimeout 3.0 do
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
  Auction.spec

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
