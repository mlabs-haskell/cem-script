{-# OPTIONS_GHC -Wno-orphans #-}

{- | Various utils to cope with `cardano-api` types
Mainly stolen from `hydra-cardano-api` and some from `atlas`
-}
module Cardano.Extras where

import Prelude

import Data.Aeson qualified as Aeson
import Data.Word (Word64)

import PlutusLedgerApi.V1.Address (Address (..), pubKeyHashAddress)
import PlutusLedgerApi.V1.Credential (
  Credential (..),
  StakingCredential (..),
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))

import Cardano.Api (
  AddressAny (..),
  AddressInEra (..),
  AddressTypeInEra (..),
  AsType (..),
  AssetId (..),
  AssetName (..),
  BabbageEra,
  BabbageEraOnwards (BabbageEraOnwardsBabbage),
  BuildTx,
  BuildTxWith (..),
  ConsensusModeParams (..),
  EpochSlots (EpochSlots),
  ExecutionUnits (..),
  HasTypeProxy (AsType),
  HashableScriptData,
  IsScriptWitnessInCtx (..),
  IsShelleyBasedEra (..),
  Key (..),
  KeyWitnessInCtx (..),
  NetworkId (..),
  PaymentKey,
  PlutusScript,
  PolicyId (..),
  Quantity (..),
  ScriptDatum (..),
  ScriptRedeemer,
  ScriptWitness (..),
  SigningKey (..),
  TextEnvelopeError (TextEnvelopeAesonDecodeError),
  TxIn,
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  UTxO (unUTxO),
  Value,
  WitCtxTxIn,
  Witness (..),
  deserialiseFromTextEnvelope,
  txOutValueToValue,
  unsafeHashableScriptData,
  valueFromList,
  verificationKeyHash,
 )
import Cardano.Api qualified as Cardano
import Cardano.Api.Byron (Hash (..))
import Cardano.Api.Ledger (StandardCrypto)
import Cardano.Api.Shelley (PlutusScriptOrReferenceInput (..), fromPlutusData, fromShelleyAddrIsSbe)
import Cardano.Crypto.Hash.Class qualified as CC
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Babbage qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Plutus.TxInfo qualified as Ledger
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map (elems)
import Data.Map qualified as Map
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), flattenValue)
import PlutusLedgerApi.V2 (ScriptHash (..), ToData (..), adaSymbol, adaToken, toData)
import PlutusTx.Builtins.Class (FromBuiltin (..))

-- Common

type Era = BabbageEra
type LedgerEra = Ledger.BabbageEra StandardCrypto

type PlutusLang = Cardano.PlutusScriptV2

plutusLang :: Cardano.PlutusScriptVersion PlutusLang
plutusLang = Cardano.PlutusScriptV2

plutusLangInEra ::
  Cardano.ScriptLanguageInEra PlutusLang Era
plutusLangInEra = Cardano.PlutusScriptV2InBabbage

-- | Orphan Instances
instance (Show Cardano.LedgerEpochInfo) where
  show _ = "LedgerEpochInfo (Church-encoded)"

deriving stock instance (Eq (SigningKey PaymentKey))

instance Semigroup (Cardano.TxOutValue Era) where
  (Cardano.TxOutValueShelleyBased era x) <> (Cardano.TxOutValueShelleyBased _era y) =
    Cardano.TxOutValueShelleyBased era (x <> y)
  (Cardano.TxOutValueByron {}) <> _ = error "pre-Shelley era TxOut is not supported"
  _ <> (Cardano.TxOutValueByron {}) = error "pre-Shelley era TxOut is not supported"

instance Monoid (Cardano.TxOutValue Era) where
  mempty = Cardano.TxOutValueShelleyBased shelleyBasedEra mempty

-- | Parsing

{- | Interpret some raw 'ByteString' as a particular 'Hash'.

NOTE: This throws if byte string has a length different that the expected
target digest length.
-}
unsafeHashFromBytes ::
  (CC.HashAlgorithm hash) =>
  ByteString ->
  CC.Hash hash a
unsafeHashFromBytes bytes =
  case CC.hashFromBytes bytes of
    Nothing ->
      error $ "unsafeHashFromBytes: failed to convert hash: " <> show bytes
    Just h ->
      h

parseSigningKeyTE :: ByteString -> Maybe (SigningKey PaymentKey)
parseSigningKeyTE bs = do
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left _ -> Nothing
    Right key -> Just key
  where
    asSigningKey :: AsType (SigningKey PaymentKey)
    asSigningKey = AsSigningKey AsPaymentKey

-- | Conversions
toPlutusKeyHash :: Hash PaymentKey -> PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) = Ledger.transKeyHash vkh

signingKeyToPKH :: SigningKey PaymentKey -> PubKeyHash
signingKeyToPKH = toPlutusKeyHash . verificationKeyHash . getVerificationKey

signingKeyToAddress :: SigningKey PaymentKey -> Address
signingKeyToAddress = pubKeyHashAddress . signingKeyToPKH

fromPlutusAddress :: NetworkId -> Address -> AddressInEra Era
fromPlutusAddress networkId plutusAddress =
  fromShelleyAddrIsSbe @Era shelleyBasedEra $
    case (addressCredential, addressStakingCredential) of
      (cred, Just (StakingHash stakeCred)) ->
        Ledger.Addr network (unsafeCredential cred) . Ledger.StakeRefBase $ unsafeCredential stakeCred
      (cred, Just (StakingPtr slot txix certix)) ->
        Ledger.Addr network (unsafeCredential cred) . Ledger.StakeRefPtr $
          Ledger.Ptr
            (fromInteger slot)
            (Ledger.TxIx $ fromInteger txix)
            (Ledger.CertIx $ fromInteger certix)
      (cred, Nothing) ->
        Ledger.Addr network (unsafeCredential cred) Ledger.StakeRefNull
  where
    network = case networkId of
      Testnet _ -> Ledger.Testnet
      Mainnet -> Ledger.Mainnet
    unsafeCredential = \case
      PubKeyCredential (PubKeyHash h) ->
        Ledger.KeyHashObj . Ledger.KeyHash . unsafeHashFromBytes $ fromBuiltin h
      ScriptCredential (ScriptHash h) ->
        Ledger.ScriptHashObj . Ledger.ScriptHash . unsafeHashFromBytes $ fromBuiltin h

    Address {addressCredential, addressStakingCredential} = plutusAddress

addressInEraToAny :: AddressInEra Era -> AddressAny
addressInEraToAny (AddressInEra ByronAddressInAnyEra a) = AddressByron a
addressInEraToAny (AddressInEra (ShelleyAddressInEra _) a) = AddressShelley a

{- | Unsafe wrap some bytes as a 'ScriptHash', relying on the fact that Plutus
is using Blake2b_224 for hashing data (according to 'cardano-ledger').

Pre-condition: the input bytestring MUST be of length 28.
-}
unsafeScriptHashFromBytes ::
  ByteString ->
  Cardano.ScriptHash
unsafeScriptHashFromBytes bytes
  | BS.length bytes /= 28 =
      error $ "unsafeScriptHashFromBytes: pre-condition failed: " <> show (BS.length bytes) <> " bytes."
  | otherwise =
      Cardano.ScriptHash
        . Ledger.ScriptHash
        $ unsafeHashFromBytes bytes

-- | Convert a plutus 'CurrencySymbol' into a cardano-api 'PolicyId'.
fromPlutusCurrencySymbol :: CurrencySymbol -> PolicyId
fromPlutusCurrencySymbol = PolicyId . unsafeScriptHashFromBytes . fromBuiltin . unCurrencySymbol

-- | Convert a plutus 'Value' into a cardano-api 'Value'.
fromPlutusValue :: Plutus.Value -> Value
fromPlutusValue plutusValue =
  valueFromList $ map convertAsset $ flattenValue plutusValue
  where
    convertAsset (cs, tk, i)
      | cs == adaSymbol && tk == adaToken = (AdaAssetId, Quantity i)
      | otherwise = (AssetId (fromPlutusCurrencySymbol cs) (toAssetName tk), Quantity i)

    -- toAssetName :: Plutus.TokenName -> AssetName
    toAssetName = AssetName . fromBuiltin . unTokenName

-- | Tx and other stuff construction
type TxInWitness = BuildTxWith BuildTx (Witness WitCtxTxIn Era)

-- | Attaching mark meaning "TxIn would be witnessed by signing key"
withKeyWitness ::
  TxIn -> (TxIn, TxInWitness)
withKeyWitness txIn =
  (txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)

mkInlineDatum :: (ToData datum) => datum -> TxOutDatum ctx Era
mkInlineDatum x =
  TxOutDatumInline BabbageEraOnwardsBabbage $
    unsafeHashableScriptData $
      fromPlutusData $
        toData $
          toBuiltinData x

{- | Construct a full script witness from a datum, a redeemer and a full
'PlutusScript'. That witness has no execution budget.
-}
mkScriptWitness ::
  forall ctx.
  PlutusScript PlutusLang ->
  ScriptDatum ctx ->
  ScriptRedeemer ->
  ScriptWitness ctx Era
mkScriptWitness script datum redeemer =
  PlutusScriptWitness
    plutusLangInEra
    plutusLang
    (PScript script)
    datum
    redeemer
    (ExecutionUnits 0 0)

toScriptData :: (ToData a) => a -> HashableScriptData
toScriptData = unsafeHashableScriptData . fromPlutusData . toData

tokenToAsset :: TokenName -> AssetName
tokenToAsset (TokenName t) = AssetName $ fromBuiltin t

mkInlinedDatumScriptWitness ::
  (ToData a) =>
  PlutusScript PlutusLang ->
  a ->
  BuildTxWith BuildTx (Witness WitCtxTxIn Era)
mkInlinedDatumScriptWitness script redeemer =
  BuildTxWith $
    ScriptWitness scriptWitnessInCtx $
      mkScriptWitness
        script
        InlineScriptDatum
        (toScriptData redeemer)

mintedTokens ::
  (ToData redeemer) =>
  PlutusScript PlutusLang ->
  redeemer ->
  [(AssetName, Quantity)] ->
  Cardano.TxMintValue BuildTx Era
mintedTokens script redeemer assets =
  TxMintValue Cardano.MaryEraOnwardsBabbage mintedTokens' mintedWitnesses'
  where
    mintedTokens' = valueFromList (fmap (first (AssetId policyId)) assets)
    mintedWitnesses' =
      BuildTxWith $ Map.singleton policyId mintingWitness
    mintingWitness :: ScriptWitness Cardano.WitCtxMint Era
    mintingWitness =
      mkScriptWitness script NoScriptDatumForMint (toScriptData redeemer)
    policyId =
      PolicyId $ Cardano.hashScript $ Cardano.PlutusScript plutusLang script

-- | Fields
txOutValue :: TxOut ctx Era -> Value
txOutValue (TxOut _ value _ _) = txOutValueToValue value

mTxOutDatum :: TxOut ctx Era -> Maybe HashableScriptData
mTxOutDatum (TxOut _ _ (TxOutDatumInline _ d) _) = Just d
mTxOutDatum _ = Nothing

utxoValue :: UTxO Era -> Value
utxoValue utxo = foldMap txOutValue $ elems $ unUTxO utxo

-- | Constants
cardanoModeParams :: ConsensusModeParams
cardanoModeParams = CardanoModeParams $ EpochSlots defaultByronEpochSlots
  where
    -- NOTE(AB): extracted from Parsers in cardano-cli, this is needed to run in 'cardanoMode' which
    -- is the default for cardano-cli
    defaultByronEpochSlots = 21600 :: Word64
