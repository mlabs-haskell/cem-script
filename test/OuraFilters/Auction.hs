{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OuraFilters.Auction (spec) where

import Cardano.CEM.Examples.Auction qualified as Auction
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.OnChain qualified as Compiled
import Control.Lens ((%~), (.~))
import Control.Monad ((>=>))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS.IO
import Data.Data (Proxy (Proxy))
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Oura qualified

-- import Oura.Config qualified as Config

import Cardano.CEM.OuraConfig qualified as OuraConfig
import OuraFilters.Mock qualified as Mock
import Plutus.Extras (scriptValidatorHash)
import PlutusLedgerApi.V1 qualified
import System.Process (ProcessHandle)
import Test.Hspec (describe, focus, it, shouldBe)
import Test.Hspec.Core.Spec (SpecM)
import Toml qualified
import Utils (SpotGarbage, withTimeout)
import Prelude

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec =
  describe "Auction example" do
    focus $ it "Catches any Auction validator transition" \spotGarbage ->
      let
        auctionAddress =
          PlutusLedgerApi.V1.Address
            auctionPaymentCredential
            Nothing
        auctionAddressBech32Text =
          Mock.shelleyAddressBech32
            . either error id
            $ Mock.plutusAddressToShelleyAddress auctionAddress
        auctionPaymentCredential =
          PlutusLedgerApi.V1.ScriptCredential auctionValidatorHash
        auctionValidatorHash =
          scriptValidatorHash
            . Compiled.cemScriptCompiled
            $ Proxy @Auction.SimpleAuction

        -- we want oura to monitor just payment credential, ignoring stake credentials
        arbitraryStakeCredential = PlutusLedgerApi.V1.StakingPtr 5 3 2

        rightTxHash =
          Mock.MkBlake2b255Hex
            "2266778888888888888888888888888888888888888888888888444444444444"
        inputFromValidator =
          emptyInputFixture auctionPaymentCredential Nothing -- (Just arbitraryStakeCredential)
        tx =
          Mock.txToBS
            . Mock.mkTxEvent
            . (Mock.inputs %~ (inputFromValidator :))
            . (Mock.hash .~ rightTxHash)
            $ Mock.arbitraryTx
        unmatchingTx =
          Mock.txToBS
            . Mock.mkTxEvent
            $ Mock.arbitraryTx
        makeConfig :: OuraConfig.SourcePath -> OuraConfig.SinkPath -> Toml.Table
        makeConfig sourcePath sinkPath =
          OuraConfig.daemonConfig
            [OuraConfig.selectByAddress auctionAddressBech32Text]
            sourcePath
            sinkPath
       in
        do
          putStrLn "Hash:"
          T.IO.putStrLn auctionAddressBech32Text
          Oura.withOura (Oura.MkWorkDir "./tmp") spotGarbage makeConfig \oura -> do
            withTimeout 6.0 do
              oura.send unmatchingTx
              oura.send tx
              msg <- oura.receive
              BS.IO.putStrLn $ "message: " <> msg
              txHash <- either error pure $ extractTxHash msg
              Mock.MkBlake2b255Hex txHash `shouldBe` rightTxHash
              oura.shutDown

emptyInputFixture ::
  PlutusLedgerApi.V1.Credential ->
  Maybe PlutusLedgerApi.V1.StakingCredential ->
  Mock.TxInput
emptyInputFixture paymentCred mstakeCred =
  Mock.MkTxInput
    { Mock._as_output =
        Mock.MkTxOutput
          { Mock._address =
              Mock.plutusAddressToOuraAddress $
                PlutusLedgerApi.V1.Address paymentCred mstakeCred
          , Mock._datum = Nothing
          , Mock._coin = 2
          , Mock._script = Nothing
          , Mock._assets = mempty
          }
    , Mock._tx_hash = Mock.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
    , Mock._output_index = 0
    , Mock._redeemer = Nothing
    }

extractTxHash :: BS.ByteString -> Either String T.Text
extractTxHash =
  Aeson.eitherDecodeStrict >=> Aeson.Types.parseEither \json -> do
    parsedTx <- json .: "record"
    parsedTx .: "hash"
