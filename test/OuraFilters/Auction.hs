{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
module OuraFilters.Auction (spec) where
import Prelude
import Utils (SpotGarbage)
import System.Process (ProcessHandle)
import Test.Hspec (describe, it, focus, HasCallStack)
import Test.Hspec.Core.Spec (SpecM)
import qualified OuraFilters.Mock as Mock
import Data.Function ((&))
import Cardano.CEM.Examples.Auction (SimpleAuctionState(NotStarted))
import qualified Cardano.CEM.Examples.Auction as Auction
import qualified PlutusLedgerApi.V1
import Control.Arrow ((>>>))
import qualified PlutusTx.AssocMap as AssocMap
import Data.Functor ((<&>))
import qualified Data.Text.Encoding as T.Encoding
import "cardano-api" Cardano.Api.Address qualified as Address
import qualified Cardano.Api.SerialiseRaw as SerialiseRaw
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Cred
import qualified Cardano.Ledger.Keys as Ledger.Keys
import qualified Cardano.Crypto.Hash as Cardano.Hash
import Data.Maybe (fromJust)
import qualified Cardano.Api.Ledger
import qualified Cardano.Ledger.Hashes
import qualified Data.Base64.Types as Base64
import qualified Data.ByteString.Base64 as BS.Base64

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec = 
  describe "Auction example" do
    focus $ it "Recognizes 'Create' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Start' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'MakeBid' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Close' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Buyout' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"

plutusAaddressToOuraAddress :: HasCallStack => PlutusLedgerApi.V1.Address -> Mock.Address
plutusAaddressToOuraAddress ( PlutusLedgerApi.V1.Address payment stake) =
  Mock.MkAddressAsBase64
  $ Base64.extractBase64
  $ BS.Base64.encodeBase64
  $ SerialiseRaw.serialiseToRawBytes
  $ Address.ShelleyAddress
    Ledger.Mainnet
    (fromJust paymentCredential)
    (fromJust stakeCredential)
  where

    credentialToCardano
      (PlutusLedgerApi.V1.PubKeyCredential
        (PlutusLedgerApi.V1.PubKeyHash pkh)) =
      Cred.KeyHashObj
        . Ledger.Keys.KeyHash
        <$> Cardano.Hash.hashFromBytes (PlutusLedgerApi.V1.fromBuiltin pkh)
    credentialToCardano
      (PlutusLedgerApi.V1.ScriptCredential
        (PlutusLedgerApi.V1.ScriptHash scriptHash)) =
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
