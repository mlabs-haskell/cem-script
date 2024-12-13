module CEM.Test.TestNFT (
  testNftPolicy,
  testNftCurrencySymbol,
  testNftAssetClass,
  testNftTokenName,
) where

import Plutus.Extras (scriptCurrencySymbol)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Value (
  AssetClass (..),
  CurrencySymbol,
  TokenName (..),
 )
import PlutusTx qualified
import PlutusTx.Prelude

testNftPolicy :: SerialisedScript
testNftPolicy =
  serialiseCompiledCode
    $ $$(PlutusTx.compile [||\(_ :: BuiltinData) (_ :: BuiltinData) -> ()||])

testNftCurrencySymbol :: CurrencySymbol
testNftCurrencySymbol = scriptCurrencySymbol testNftPolicy

testNftTokenName :: TokenName
testNftTokenName = TokenName "Mona Lisa by Leonardo da Vinci"

testNftAssetClass :: AssetClass
testNftAssetClass =
  AssetClass (testNftCurrencySymbol, testNftTokenName)
