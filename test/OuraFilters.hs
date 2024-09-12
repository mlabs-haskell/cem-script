{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OuraFilters (ouraFiltersSpec) where

import Prelude
import Oura (Oura (send, receive, shutDown))
import Oura qualified
import Test.Hspec (Spec, it, focus, shouldBe)
import Control.Monad ((>=>))
import qualified Data.Text as T
import Utils qualified
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import OuraFilters.Auction qualified
import qualified OuraFilters.Mock as Mock
import Data.Function ((&))
import Control.Lens ((.~), ix)
import qualified PlutusLedgerApi.V1 as V1

exampleMatchingTx :: Mock.TxEvent
exampleMatchingTx =
  exampleTx
    & Mock.parsed_tx . Mock.inputs . ix 0 . Mock.as_output . Mock.address .~ inputAddress
  where
    inputAddress = Mock.MkAddressAsBase64 "AZSTMVzZLrXYxDBOZ7fhauNtYdNFAmlGV4EaLI4ze2LP/2QDoGo6y8NPjEYAPGn+eaNijO+pxHJR"

exampleTx :: Mock.TxEvent
exampleTx = Mock.mkTxEvent $ Mock.arbitraryTx
  & Mock.inputs .~ [
    Mock.MkTxInput
      { Mock._tx_hash = Mock.Mk32BitBase16Hash "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
      , Mock._output_index = 5
      , Mock._as_output = out
      , Mock._redeemer = Nothing
      }
  ]
  & Mock.outputs .~ [out]
  & Mock.txCollateral . Mock.collateral_return . Mock.coin .~ 25464
  & Mock.txCollateral . Mock.total_collateral .~ 2555
  & Mock.fee .~ 967
  & Mock.validity .~ Mock.MkTxValidity { Mock._start = 324, Mock._ttl = 323 }

  where
    out = Mock.MkTxOutput
      { Mock._address = Mock.MkAddressAsBase64 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
      , Mock._coin = 254564
      , Mock._assets = []
      , Mock._datum = Just
            $ Mock.encodePlutusData
            $ V1.List
              [ V1.Map
                [ (V1.I 2, V1.I 33)
                ]
              , V1.Constr 3 [V1.I 288]
              , V1.I 34
              ]
      , Mock._script = Nothing
      }

ouraFiltersSpec :: Spec
ouraFiltersSpec = Utils.killProcessesOnError do
  focus $ it "Oura filters match tx it have to match, and don't match other" \spotGarbage -> do
    Oura.withOura (Oura.MkWorkDir "./tmp") spotGarbage \oura -> do
      let
        tx = Mock.txToText exampleTx
        matchingTx = Mock.txToText exampleMatchingTx
      oura.send tx
      -- _ <- oura.receive
      oura.send matchingTx
      Right outTxHash
        <- extractOutputTxHash <$> oura.receive
      Right inputTxHash
        <- pure $ extractInputTxHash matchingTx
      outTxHash `shouldBe` inputTxHash
      oura.shutDown
  OuraFilters.Auction.spec

extractInputTxHash :: T.Text -> Either String T.Text
extractInputTxHash = Aeson.eitherDecodeStrictText >=> Aeson.parseEither \json -> do
  parsedTx <- json .: "parsed_tx"
  parsedTx .: "hash"

extractOutputTxHash :: T.Text -> Either String T.Text
extractOutputTxHash = Aeson.eitherDecodeStrictText >=> Aeson.parseEither \json -> do
  parsedTx <- json .: "record"
  parsedTx .: "hash"