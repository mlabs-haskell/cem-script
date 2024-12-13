module Plutus.Extras where

import PlutusTx.Prelude

import Cardano.Api (
  Script (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  hashScript,
 )
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V2 (ScriptHash (..), UnsafeFromData (..))

import Cardano.Extras
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

{- | Wrap a typed validator to get the basic `ValidatorType` signature which can
be passed to `PlutusTx.compile`.
REVIEW: There might be better ways to name this than "wrap"
-}
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
wrapValidator f d r c =
  check $ f datum redeemer context
  where
    datum = unsafeFromBuiltinData d
    redeemer = unsafeFromBuiltinData r
    context = unsafeFromBuiltinData c
{-# INLINEABLE wrapValidator #-}

{- | Compute the on-chain 'ScriptHash' for a given serialised plutus script. Use
this to refer to another validator script.
-}
scriptValidatorHash :: SerialisedScript -> ScriptHash
scriptValidatorHash =
  ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript plutusLang
    . PlutusScriptSerialised @PlutusLang

scriptCurrencySymbol :: SerialisedScript -> CurrencySymbol
scriptCurrencySymbol script =
  case scriptValidatorHash script of
    ScriptHash hash -> CurrencySymbol hash
