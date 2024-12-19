module Cardano.CEM.Address (
  cemScriptAddress,
  cemScriptPlutusCredential,
  cemScriptPlutusAddress,
  plutusAddressToShelleyAddress,
) where

import Cardano.Api qualified as C
import Cardano.Api.Address qualified as C (Address (..))
import Cardano.Api.Ledger qualified as C
import Cardano.CEM.OnChain (CEMScriptCompiled (cemScriptCompiled))
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Hashes qualified as L
import Cardano.Ledger.Keys qualified as L
import Data.Proxy (Proxy)
import Plutarch.LedgerApi (scriptHash)
import Plutarch.Script (serialiseScript)
import Plutus.Extras (scriptValidatorHash)
import PlutusLedgerApi.V1 qualified as P
import PlutusLedgerApi.V1.Address qualified as P (scriptHashAddress)
import Prelude

cemScriptAddress ::
  forall script.
  (CEMScriptCompiled script) =>
  C.Network ->
  Proxy script ->
  Either String (C.Address C.ShelleyAddr)
cemScriptAddress network =
  plutusAddressToShelleyAddress network
    . flip P.Address Nothing
    . cemScriptPlutusCredential

{-# INLINEABLE cemScriptPlutusAddress #-}
cemScriptPlutusAddress ::
  forall script. (CEMScriptCompiled script) => Proxy script -> P.Address
cemScriptPlutusAddress =
  P.scriptHashAddress
    . scriptValidatorHash
    . serialiseScript
    . cemScriptCompiled

cemScriptPlutusCredential ::
  forall script.
  (CEMScriptCompiled script) =>
  Proxy script ->
  P.Credential
cemScriptPlutusCredential =
  P.ScriptCredential
    . scriptHash
    . cemScriptCompiled

plutusAddressToShelleyAddress ::
  L.Network ->
  P.Address ->
  Either String (C.Address C.ShelleyAddr)
plutusAddressToShelleyAddress network (P.Address payment stake) = do
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
  pure $ C.ShelleyAddress network paymentCred stakeCred
  where
    credentialToCardano
      ( P.PubKeyCredential
          (P.PubKeyHash pkh)
        ) =
        L.KeyHashObj
          . L.KeyHash
          <$> Crypto.hashFromBytes
            (P.fromBuiltin pkh)
    credentialToCardano
      ( P.ScriptCredential
          (P.ScriptHash hash')
        ) =
        L.ScriptHashObj
          . L.ScriptHash
          <$> Crypto.hashFromBytes
            (P.fromBuiltin hash')

    paymentCredential = credentialToCardano payment
    stakeCredential = case stake of
      Nothing -> Just L.StakeRefNull
      Just ref -> case ref of
        P.StakingHash cred ->
          L.StakeRefBase
            <$> credentialToCardano cred
        P.StakingPtr slotNo txIx sertId ->
          Just $
            L.StakeRefPtr $
              L.Ptr
                (L.SlotNo $ fromInteger slotNo)
                (L.TxIx $ fromInteger txIx)
                (L.CertIx $ fromInteger sertId)
