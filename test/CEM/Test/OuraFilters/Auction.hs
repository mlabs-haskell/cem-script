{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CEM.Test.OuraFilters.Auction (spec) where

import CEM.Example.Auction qualified as Auction
import CEM.Example.Compiled ()
import CEM.Test.Oura.Communication qualified as Oura
import CEM.Test.OuraFilters.Mock qualified as Mock
import CEM.Test.Utils (SpotGarbage, withTimeout)
import Cardano.CEM hiding (error) -- FIXME:
import Cardano.CEM.Indexing.Oura qualified as OuraConfig
import Cardano.CEM.Indexing.Tx qualified as Tx
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
        auctionPaymentCredential = scriptCredential $ Proxy @Auction.SimpleAuction

        -- we want oura to monitor just payment credential, ignoring stake credentials
        arbitraryStakeCredential = PlutusLedgerApi.V1.StakingPtr 5 3 2

        rightTxHash =
          Tx.MkBlake2b255Hex
            "2266778888888888888888888888888888888888888888888888444444444444"
        inputFromValidator =
          emptyInputFixture auctionPaymentCredential (Just arbitraryStakeCredential)
        tx =
          Mock.txToBS
            . Mock.mkTxEvent
            . (Tx.inputs %~ (inputFromValidator :))
            . (Tx.hash .~ rightTxHash)
            $ Tx.arbitraryTx
        unmatchingTx =
          Mock.txToBS
            . Mock.mkTxEvent
            $ Tx.arbitraryTx
        makeConfig source sink =
          either error id $
            OuraConfig.ouraMonitoringScript (Proxy @Auction.SimpleAuction) Ledger.Mainnet source sink
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
                Tx.MkBlake2b255Hex txHash `shouldBe` rightTxHash
                oura.shutDown

emptyInputFixture ::
  PlutusLedgerApi.V1.Credential ->
  Maybe PlutusLedgerApi.V1.StakingCredential ->
  Tx.TxInput
emptyInputFixture paymentCred mstakeCred =
  Tx.MkTxInput
    { Tx._as_output =
        Tx.MkTxOutput
          { Tx._address =
              Tx.plutusAddressToOuraAddress $
                PlutusLedgerApi.V1.Address paymentCred mstakeCred
          , Tx._datum = Nothing
          , Tx._coin = 2
          , Tx._script = Nothing
          , Tx._assets = mempty
          }
    , Tx._tx_hash = Tx.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
    , Tx._output_index = 0
    , Tx._redeemer = Nothing
    }

extractTxHash :: BS.ByteString -> Either String T.Text
extractTxHash =
  Aeson.eitherDecodeStrict >=> Aeson.Types.parseEither \json -> do
    parsedTx <- json .: "record"
    parsedTx .: "hash"
