module Cardano.CEM.Address (
  AddressBech32 (MkAddressBech32, unAddressBech32),
  cardanoAddressBech32,
  scriptCredential,
  scriptCardanoAddress,
  plutusAddressToShelleyAddress,
) where

import Cardano.Api qualified
import Cardano.Api.Address qualified
import Cardano.Api.Ledger qualified
import Cardano.CEM.OnChain qualified as Compiled
import Cardano.Crypto.Hash qualified as Cardano.Hash
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Cred
import Cardano.Ledger.Hashes qualified
import Cardano.Ledger.Keys qualified as Ledger.Keys
import Data.Proxy (Proxy)
import Data.String (IsString)
import Data.Text qualified as T
import Plutarch.LedgerApi (scriptHash)
import PlutusLedgerApi.V1 qualified
import Prelude

newtype AddressBech32 = MkAddressBech32 {unAddressBech32 :: T.Text}
  deriving newtype (Eq, Show, IsString)

cardanoAddressBech32 :: Cardano.Api.Address Cardano.Api.ShelleyAddr -> AddressBech32
cardanoAddressBech32 = MkAddressBech32 . Cardano.Api.serialiseToBech32

scriptCardanoAddress ::
  forall script.
  (Compiled.CEMScriptCompiled script) =>
  Proxy script ->
  Cardano.Api.Ledger.Network ->
  Either String (Cardano.Api.Address Cardano.Api.ShelleyAddr)
scriptCardanoAddress p network =
  plutusAddressToShelleyAddress network
    . flip PlutusLedgerApi.V1.Address Nothing
    . scriptCredential
    $ p

scriptCredential ::
  forall script.
  (Compiled.CEMScriptCompiled script) =>
  Proxy script ->
  PlutusLedgerApi.V1.Credential
scriptCredential p =
  PlutusLedgerApi.V1.ScriptCredential
    . scriptHash
    . Compiled.cemScriptCompiled
    $ p

plutusAddressToShelleyAddress ::
  Cardano.Api.Ledger.Network ->
  PlutusLedgerApi.V1.Address ->
  Either String (Cardano.Api.Address Cardano.Api.ShelleyAddr)
plutusAddressToShelleyAddress network (PlutusLedgerApi.V1.Address payment stake) = do
  paymentCred <-
    maybe
      (Left "plutusAddressToShelleyAddress:can't decode payment credential")
      Right
      paymentCredential
  stakeCred <-
    maybe
      (Left "plutusAddressToShelleyAddress:can't decode stake credential")
      Right
      stakeCredential
  pure $ Cardano.Api.Address.ShelleyAddress network paymentCred stakeCred
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
          (PlutusLedgerApi.V1.ScriptHash hash)
        ) =
        Cred.ScriptHashObj
          . Cardano.Ledger.Hashes.ScriptHash
          <$> Cardano.Hash.hashFromBytes
            (PlutusLedgerApi.V1.fromBuiltin hash)

    paymentCredential = credentialToCardano payment
    stakeCredential = case stake of
      Nothing -> Just Cardano.Api.Ledger.StakeRefNull
      Just ref -> case ref of
        PlutusLedgerApi.V1.StakingHash cred ->
          Cardano.Api.Ledger.StakeRefBase
            <$> credentialToCardano cred
        PlutusLedgerApi.V1.StakingPtr slotNo txIx sertId ->
          Just $
            Cardano.Api.Ledger.StakeRefPtr $
              Cred.Ptr
                (Ledger.SlotNo $ fromInteger slotNo)
                (Ledger.TxIx $ fromInteger txIx)
                (Ledger.CertIx $ fromInteger sertId)
