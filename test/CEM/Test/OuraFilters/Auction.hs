{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CEM.Test.OuraFilters.Auction (spec) where

import CEM.Example.Auction qualified as Auction
import CEM.Example.Compiled ()
import CEM.Test.Oura.Communication qualified as Oura
import CEM.Test.OuraFilters.Mock qualified as Mock
import CEM.Test.Utils (SpotGarbage, withTimeout)
import Cardano.CEM hiding (error)
import Cardano.CEM.Indexing
import Cardano.Ledger.BaseTypes qualified as Ledger
import Control.Lens ((%~), (.~))
import Control.Monad ((>=>))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString qualified as BS
import Data.Data (Proxy (Proxy))
import Data.Text qualified as T
import PlutusLedgerApi.V1 qualified
import System.Process (ProcessHandle)
import Test.Hspec (describe, focus, it, shouldBe)
import Test.Hspec.Core.Spec (SpecM)
import Prelude

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec =
  describe "Auction example" do
    focus $ it "Catches any Auction validator transition" \spotGarbage ->
      let
        auctionPaymentCredential = cemScriptPlutusCredential $ Proxy @Auction.SimpleAuction

        -- we want oura to monitor just payment credential, ignoring stake credentials
        arbitraryStakeCredential = PlutusLedgerApi.V1.StakingPtr 5 3 2

        rightTxHash =
          MkBlake2b255Hex
            "2266778888888888888888888888888888888888888888888888444444444444"
        inputFromValidator =
          emptyInputFixture auctionPaymentCredential (Just arbitraryStakeCredential)
        tx =
          Mock.txToBS
            . Mock.mkTxEvent
            . (inputs %~ (inputFromValidator :))
            . (hash .~ rightTxHash)
            $ arbitraryTx
        unmatchingTx =
          Mock.txToBS
            . Mock.mkTxEvent
            $ arbitraryTx
        makeConfig source sink =
          either error id $
            ouraMonitoringScript (Proxy @Auction.SimpleAuction) Ledger.Mainnet source sink
       in
        do
          Oura.withOura
            (Oura.MkWorkDir "./tmp")
            spotGarbage
            makeConfig
            \oura -> do
              withTimeout 6.0 do
                putStrLn "------------------------------"
                print tx
                oura.send unmatchingTx
                oura.send tx
                msg <- oura.receive
                txHash <- either error pure $ extractTxHash msg
                MkBlake2b255Hex txHash `shouldBe` rightTxHash
                oura.shutDown

emptyInputFixture ::
  PlutusLedgerApi.V1.Credential ->
  Maybe PlutusLedgerApi.V1.StakingCredential ->
  TxInput
emptyInputFixture paymentCred mstakeCred =
  MkTxInput
    { _as_output =
        MkTxOutput
          { _address =
              plutusAddressToOuraAddress $
                PlutusLedgerApi.V1.Address paymentCred mstakeCred
          , _datum = Nothing
          , _coin = 2
          , _script = Nothing
          , _assets = mempty
          }
    , _tx_hash = MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
    , _output_index = 0
    , _redeemer = Nothing
    }

extractTxHash :: BS.ByteString -> Either String T.Text
extractTxHash =
  Aeson.eitherDecodeStrict >=> Aeson.Types.parseEither \json -> do
    parsedTx <- json .: "record"
    parsedTx .: "hash"
