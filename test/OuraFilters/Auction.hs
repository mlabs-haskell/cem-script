{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module OuraFilters.Auction (spec) where

import Cardano.Api.Ledger qualified
import Cardano.Api.SerialiseRaw qualified as SerialiseRaw
import Cardano.CEM (CEMScriptDatum)
import Cardano.CEM.Examples.Auction (SimpleAuctionState (NotStarted))
import Cardano.CEM.Examples.Auction qualified as Auction
import Cardano.Crypto.Hash qualified as Cardano.Hash
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Cred
import Cardano.Ledger.Hashes qualified
import Cardano.Ledger.Keys qualified as Ledger.Keys
import Control.Arrow ((>>>))
import Control.Lens ((%~), (.~), (^.))
import Data.Aeson qualified as Aeson
import Data.Base16.Types qualified as Base16.Types
import Data.Base64.Types qualified as Base64
import Data.Base64.Types qualified as Base64.Types
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base32 qualified as Base32
import Data.ByteString.Base64 qualified as BS.Base64
import Data.ByteString.Base64 qualified as Base64
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Oura qualified
import OuraFilters.Mock qualified as Mock
import PlutusLedgerApi.V1 qualified
import PlutusLedgerApi.V1.Value qualified as V1.Value
import PlutusTx.AssocMap qualified as AssocMap
import System.Process (ProcessHandle)
import Test.Hspec (HasCallStack, describe, focus, it, shouldBe)
import Test.Hspec.Core.Spec (SpecM)
import Utils (SpotGarbage, resultToEither)
import "cardano-api" Cardano.Api.Address qualified as Address
import Prelude

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec =
  describe "Auction example" do
    focus $ it "Recognizes 'Create' transition" \spotGarbage -> do
      Oura.withOura (Oura.MkWorkDir "./tmp") spotGarbage \oura -> do
        let
          params =
            Auction.MkAuctionParams
              { seller = "e01bb07f0cd514c0b8b73572e8c5e7492449c5f68702fdac758225f4" -- "e01bb07f0cd514c0b8b73572e8c5e7492449c5f68702fdac758225f4"
              , lot =
                  V1.Value.assetClassValue
                    ( V1.Value.assetClass
                        "94906060606060606060606060606060606060606060606969669696"
                        "fea6"
                    )
                    4
              }
        flip shouldBe 28 $
          BS.length
          -- \$ Base16.decodeBase16
          -- \$ Base16.Types.assertBase16
          $
            PlutusLedgerApi.V1.fromBuiltin $
              PlutusLedgerApi.V1.getPubKeyHash params.seller
        let
          rightTxHash =
            Mock.MkBlake2b255Hex
              "2266778888888888888888888888888888888888888888888888444444444444"
          tx =
            Mock.txToText $
              Mock.mkTxEvent $
                Mock.hash .~ rightTxHash $
                  createTxMock params
          unmatchingTx = Mock.txToText $ Mock.mkTxEvent Mock.arbitraryTx
        putStrLn "evaluating"
        print $
          plutusAaddressToOuraAddress $
            PlutusLedgerApi.V1.Address
              (PlutusLedgerApi.V1.PubKeyCredential params.seller)
              Nothing
        putStrLn "good"
        oura.send unmatchingTx
        putStrLn "Sent1"
        oura.send tx
        putStrLn "Sent2"
        Right txEvent <-
          Aeson.eitherDecodeStrictText @Mock.TxEvent
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

plutusAaddressToOuraAddress :: (HasCallStack) => PlutusLedgerApi.V1.Address -> Mock.Address
plutusAaddressToOuraAddress (PlutusLedgerApi.V1.Address payment stake) =
  Mock.MkAddressAsBase64 $
    Base64.extractBase64 $
      BS.Base64.encodeBase64 $
        SerialiseRaw.serialiseToRawBytes $
          Address.ShelleyAddress
            Ledger.Mainnet
            ( fromMaybe
                (error "plutusAaddressToOuraAddress:can't decode payment credential")
                paymentCredential
            )
            ( fromMaybe
                (error "plutusAaddressToOuraAddress:can't decode stake credential")
                stakeCredential
            )
  where
    credentialToCardano
      ( PlutusLedgerApi.V1.PubKeyCredential
          (PlutusLedgerApi.V1.PubKeyHash pkh)
        ) =
        Cred.KeyHashObj
          . Ledger.Keys.KeyHash
          <$> Cardano.Hash.hashFromBytes
            (PlutusLedgerApi.V1.fromBuiltin pkh)
    credentialToCardano
      ( PlutusLedgerApi.V1.ScriptCredential
          (PlutusLedgerApi.V1.ScriptHash scriptHash)
        ) =
        Cred.ScriptHashObj
          . Cardano.Ledger.Hashes.ScriptHash
          <$> Cardano.Hash.hashFromBytes
            (PlutusLedgerApi.V1.fromBuiltin scriptHash)

    paymentCredential = credentialToCardano payment
    stakeCredential = case stake of
      Nothing -> Just Cardano.Api.Ledger.StakeRefNull
      Just ref -> case ref of
        PlutusLedgerApi.V1.StakingHash cred ->
          Cardano.Api.Ledger.StakeRefBase
            <$> credentialToCardano cred
        PlutusLedgerApi.V1.StakingPtr {} ->
          error "Staking pointers are not supported"

createTxMock :: Auction.SimpleAuctionParams -> Mock.Tx
createTxMock params =
  Mock.arbitraryTx
    & Mock.inputs %~ (:) input
    & Mock.outputs %~ (:) output
  where
    input =
      Mock.MkTxInput
        { Mock._as_output =
            Mock.MkTxOutput
              { Mock._address =
                  plutusAaddressToOuraAddress $
                    PlutusLedgerApi.V1.Address
                      (PlutusLedgerApi.V1.PubKeyCredential params.seller)
                      Nothing
              , Mock._datum = Nothing -- any datum
              , Mock._coin = 2
              , Mock._script = Nothing
              , Mock._assets = valueToMultiAsset params.lot
              }
        , Mock._tx_hash = Mock.MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
        , Mock._output_index = 0
        , Mock._redeemer =
            Just $
              Mock.MkRedeemer
                { _purpose = Mock.PURPOSE_SPEND
                , datum =
                    Mock.encodePlutusData $
                      PlutusLedgerApi.V1.toData Auction.Create
                }
        }
    output =
      Mock.MkTxOutput
        { Mock._address =
            plutusAaddressToOuraAddress $
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
    valueToMultiAsset :: PlutusLedgerApi.V1.Value -> [Mock.Multiasset]
    valueToMultiAsset =
      PlutusLedgerApi.V1.getValue >>> AssocMap.toList >>> fmap \(cs, tokens) ->
        Mock.MkMultiasset
          { Mock._policy_id =
              T.Encoding.decodeUtf8 $
                PlutusLedgerApi.V1.fromBuiltin $
                  PlutusLedgerApi.V1.unCurrencySymbol cs
          , Mock.assets =
              AssocMap.toList tokens <&> \(tn, amt) ->
                Mock.MkAsset
                  { Mock._name =
                      T.Encoding.decodeUtf8 $
                        PlutusLedgerApi.V1.fromBuiltin $
                          PlutusLedgerApi.V1.unTokenName tn
                  , Mock._output_coin = amt -- positive
                  , Mock._mint_coin = 1
                  }
          , Mock.redeemer = Nothing
          }

    outputState :: CEMScriptDatum Auction.SimpleAuction
    outputState = (Auction.NoControl, params, Auction.NotStarted)
