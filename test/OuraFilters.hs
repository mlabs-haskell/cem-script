{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OuraFilters (ouraFiltersSpec) where

import Prelude
import Oura (Oura (send, receive, shutDown))
import Oura qualified
import Test.Hspec (Spec, it, focus, shouldBe, describe)
import Control.Monad ((>=>))
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Utils qualified
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import OuraFilters.Auction qualified

exampleTx :: IO T.Text
exampleTx = T.IO.readFile "./mocks/tx.json"

exampleMatchingTx :: IO T.Text
exampleMatchingTx = T.IO.readFile "./mocks/matchingTx.json"

ouraFiltersSpec :: Spec
ouraFiltersSpec = Utils.killProcessesOnError do
  focus $ it "Oura filters match tx it have to match, and don't match other" \spotGarbage -> do
    Oura.withOura (Oura.MkWorkDir "./tmp") spotGarbage \oura -> do
      tx <- exampleTx
      matchingTx <- exampleMatchingTx
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