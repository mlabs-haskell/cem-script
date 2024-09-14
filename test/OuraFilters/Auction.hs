{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module OuraFilters.Auction (spec) where

import Cardano.CEM (CEMScriptDatum)
import Cardano.CEM.Examples.Auction qualified as Auction
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.OnChain qualified as Compiled
import Control.Arrow ((>>>))
import Control.Lens ((%~), (.~), (?~), (^.))
import Data.Aeson qualified as Aeson
import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import Data.Functor (void, (<&>))
import Oura qualified
import OuraFilters.Mock qualified as Mock
import Plutus.Extras (scriptValidatorHash)
import PlutusLedgerApi.V1 qualified
import PlutusLedgerApi.V1.Value qualified as V1.Value
import PlutusTx.AssocMap qualified as AssocMap
import System.Process (ProcessHandle)
import System.Timeout (timeout)
import Test.Hspec (describe, focus, it, shouldBe)
import Test.Hspec.Core.Spec (SpecM)
import Utils (SpotGarbage, withTimeout)
import Prelude

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec =
  --  @(SpotGarbage IO ProcessHandle)
  describe "Auction example" do
    focus $ it "Catches any Auction validator transition" \spotGarbage -> do
      Oura.withOura @() (Oura.MkWorkDir "./tmp") spotGarbage \oura -> do
        let
          auctionPaymentCredential =
            PlutusLedgerApi.V1.ScriptCredential
              . scriptValidatorHash
              . Compiled.cemScriptCompiled
              $ Proxy @Auction.SimpleAuction
          -- we want oura to monitor just payment credential, ignoring stake credentials
          arbitraryStakeCredential = PlutusLedgerApi.V1.StakingPtr 5 3 2
          defaultTx = Mock.arbitraryTx
          rightTxHash =
            Mock.MkBlake2b255Hex
              "2266778888888888888888888888888888888888888888888888444444444444"
          inputFromValidator =
            emptyInputFixture auctionPaymentCredential (Just arbitraryStakeCredential)

          tx =
            Mock.txToBS
              . Mock.mkTxEvent
              . (Mock.inputs %~ (inputFromValidator :))
              . (Mock.hash .~ rightTxHash)
              $ defaultTx
          unmatchingTx =
            Mock.txToBS
              . Mock.mkTxEvent
              $ Mock.arbitraryTx

        withTimeout 3.0 do
          oura.send unmatchingTx
          oura.send tx
          Right txEvent <-
            Aeson.eitherDecodeStrict @Mock.TxEvent
              <$> oura.receive
          (txEvent ^. Mock.parsed_tx . Mock.hash) `shouldBe` rightTxHash
          oura.shutDown
    it "Recognizes 'Create' transition" \spotGarbage -> do
      Oura.withOura (Oura.MkWorkDir "./tmp") spotGarbage \oura -> do
        let
          params =
            Auction.MkAuctionParams
              { seller = "ab0baab0baab0baab0baab0baab0ba00000000000004444444444444"
              , lot =
                  V1.Value.assetClassValue
                    ( V1.Value.assetClass
                        "eeeeeeeeeeffffffffaaaaaaa4444444444444444444444444444444"
                        ""
                    )
                    4
              }
          rightTxHash =
            Mock.MkBlake2b255Hex
              "2266778888888888888888888888888888888888888888888888444444444444"
          tx =
            Mock.txToBS $
              Mock.mkTxEvent $
                Mock.hash .~ rightTxHash $
                  createTxMock params
          unmatchingTx = Mock.txToBS $ Mock.mkTxEvent Mock.arbitraryTx
        withTimeout 3.0 do
          oura.send unmatchingTx
          oura.send tx
          Right txEvent <-
            Aeson.eitherDecodeStrict @Mock.TxEvent
              <$> oura.receive
          (txEvent ^. Mock.parsed_tx . Mock.hash) `shouldBe` rightTxHash
          oura.shutDown
    it "Recognizes 'Start' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'MakeBid' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Close' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Buyout' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"

createTxMock :: Auction.SimpleAuctionParams -> Mock.Tx
createTxMock params =
  Mock.arbitraryTx
    & Mock.inputs %~ (:) input
    & Mock.outputs %~ (:) output
  where
    input =
      emptyInputFixture
        (PlutusLedgerApi.V1.PubKeyCredential params.seller)
        Nothing
        & Mock.as_output . Mock.assets .~ valueToMultiAsset params.lot
        & Mock.redeemer
          ?~ Mock.MkRedeemer
            { _purpose = Mock.PURPOSE_SPEND
            , datum =
                Mock.encodePlutusData $
                  PlutusLedgerApi.V1.toData Auction.Create
            }

    output =
      Mock.MkTxOutput
        { Mock._address =
            Mock.plutusAddressToOuraAddress $
              PlutusLedgerApi.V1.Address
                (PlutusLedgerApi.V1.PubKeyCredential params.seller)
                Nothing
        , Mock._datum =
            Just $
              Mock.encodePlutusData $
                PlutusLedgerApi.V1.toData outputState
        , Mock._coin = 2
        , Mock._script = Nothing
        , Mock._assets = valueToMultiAsset params.lot
        }

    outputState :: CEMScriptDatum Auction.SimpleAuction
    outputState = (Auction.NoControl, params, Auction.NotStarted)

valueToMultiAsset :: PlutusLedgerApi.V1.Value -> [Mock.Multiasset]
valueToMultiAsset =
  PlutusLedgerApi.V1.getValue >>> AssocMap.toList >>> fmap \(cs, tokens) ->
    Mock.MkMultiasset
      { Mock._policy_id = Mock.serialiseCurrencySymbol cs
      , Mock.assets =
          AssocMap.toList tokens <&> \(tn, amt) ->
            Mock.MkAsset
              { Mock._name =
                  Mock.serialiseAsHex $
                    PlutusLedgerApi.V1.unTokenName tn
              , Mock._output_coin = amt -- positive
              , Mock._mint_coin = 1
              }
      }

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
