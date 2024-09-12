{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module OuraFilters.Auction (spec) where

import Cardano.Api.Ledger qualified
import Cardano.Api.SerialiseRaw qualified as SerialiseRaw
import Cardano.CEM.Examples.Auction (SimpleAuctionState (NotStarted))
import Cardano.CEM.Examples.Auction qualified as Auction
import Cardano.Crypto.Hash qualified as Cardano.Hash
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Cred
import Cardano.Ledger.Hashes qualified
import Cardano.Ledger.Keys qualified as Ledger.Keys
import Control.Arrow ((>>>))
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64 qualified as BS.Base64
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Text.Encoding qualified as T.Encoding
import OuraFilters.Mock qualified as Mock
import PlutusLedgerApi.V1 qualified
import PlutusTx.AssocMap qualified as AssocMap
import System.Process (ProcessHandle)
import Test.Hspec (HasCallStack, describe, focus, it)
import Test.Hspec.Core.Spec (SpecM)
import Utils (SpotGarbage)
import "cardano-api" Cardano.Api.Address qualified as Address
import Prelude

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec =
  describe "Auction example" do
    it "Recognizes 'Create' transition" \spotGarbage -> do
      let
        addr =
          PlutusLedgerApi.V1.Address
            ( PlutusLedgerApi.V1.PubKeyCredential
                (PlutusLedgerApi.V1.PubKeyHash "e628bfd68c07a7a38fcd7d8df650812a9dfdbee54b1ed4c25c87ffbf")
            )
            Nothing
      print $ PlutusLedgerApi.V1.toBuiltinData addr
      fail @IO @() "Not implemented"
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
            (fromJust paymentCredential)
            (fromJust stakeCredential)
  where
    credentialToCardano
      ( PlutusLedgerApi.V1.PubKeyCredential
          (PlutusLedgerApi.V1.PubKeyHash pkh)
        ) =
        Cred.KeyHashObj
          . Ledger.Keys.KeyHash
          <$> Cardano.Hash.hashFromBytes (PlutusLedgerApi.V1.fromBuiltin pkh)
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
    & undefined
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
        , Mock._tx_hash = Mock.Mk32BitBase16Hash "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
        , Mock._output_index = 0
        , Mock._redeemer = undefined
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

    inputState = Nothing
    outputState = Just NotStarted
