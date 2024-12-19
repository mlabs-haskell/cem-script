{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

{- | Cardano transactions as they are represented by Oura.
Source: https://docs.rs/utxorpc-spec/latest/utxorpc_spec/utxorpc/v1alpha/cardano/struct.Tx.html
-}
module Cardano.CEM.Indexing.Tx where

import Cardano.Api qualified as C
import Cardano.Api.Address qualified as C (Address (..))
import Cardano.CEM.Address (plutusAddressToShelleyAddress)
import Cardano.Extras (Era)
import Cardano.Ledger.BaseTypes qualified as L
import Control.Lens.TH (makeLenses, makeLensesFor)
import Control.Monad ((<=<))
import Data.Aeson (KeyValue ((.=)))
import Data.Aeson qualified as Aeson
import Data.Base16.Types qualified as B16
import Data.Base64.Types qualified as B64
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import GHC.Generics (Generic (Rep))
import GHC.Stack.Types (HasCallStack)
import PlutusLedgerApi.V1 qualified as P
import Safe
import Prelude

-- Core datatypes

newtype WithoutUnderscore a = MkWithoutUnderscore a
  deriving newtype (Generic)

withoutLeadingUnderscore :: Aeson.Options
withoutLeadingUnderscore =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
        '_' : fieldName -> fieldName
        fieldName -> fieldName
    }
instance
  ( Generic a
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (GHC.Generics.Rep a)
  ) =>
  Aeson.ToJSON (WithoutUnderscore a)
  where
  toJSON = Aeson.genericToJSON withoutLeadingUnderscore

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.FromJSON (WithoutUnderscore a) where
  parseJSON = Aeson.genericParseJSON withoutLeadingUnderscore

newtype Address = MkAddressAsBase64 {_addressL :: T.Text}
  deriving newtype (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)
makeLenses ''Address

-- 32B long
newtype Hash32 = MkBlake2b255Hex {unHash32 :: T.Text}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)
  deriving newtype (Aeson.FromJSON)
makeLenses ''Hash32

-- 28B long
newtype Hash28 = MkBlake2b244Hex {unHash28 :: T.Text}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)
  deriving newtype (Aeson.FromJSON)
makeLenses ''Hash28

data Asset = MkAsset
  { _name :: T.Text
  , _output_coin :: Integer -- positive
  , _mint_coin :: Integer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Asset)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Asset)
makeLenses ''Asset

data Multiasset = MkMultiasset
  { _policy_id :: Hash28
  , assets :: [Asset]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Multiasset)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Multiasset)
makeLenses ''Multiasset
makeLensesFor
  [ ("assets", "multiassetAssets")
  ]
  ''Multiasset

newtype PlutusData = MkPlutusData {_plutusData :: Aeson.Value}
  deriving newtype (Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)
makeLenses ''PlutusData

data Purpose
  = PURPOSE_UNSPECIFIED
  | PURPOSE_SPEND
  | PURPOSE_MINT
  | PURPOSE_CERT
  | PURPOSE_REWARD
  deriving stock (Show, Enum, Bounded)

instance Aeson.FromJSON Purpose where
  parseJSON =
    maybe (fail "There is no Purpose case with this Id") pure
      . toEnumMay
      <=< Aeson.parseJSON @Int

instance Aeson.ToJSON Purpose where
  toJSON = Aeson.toJSON @Int . fromEnum

data Datum = MkDatum
  { hash :: Hash32
  , _payload :: PlutusData
  , _original_cbor :: T.Text
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Datum)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Datum)
makeLenses ''Datum
makeLensesFor [("hash", "datumHash")] ''Datum

data Redeemer = MkRedeemer
  { _purpose :: Purpose
  , payload :: PlutusData
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Redeemer)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Redeemer)
makeLenses ''Redeemer
makeLensesFor [("payload", "redeemerPayload")] ''Redeemer

data TxOutput = MkTxOutput
  { _address :: Address
  , _coin :: Integer
  , _assets :: [Multiasset]
  , _datum :: Maybe Datum
  , _script :: Maybe Aeson.Value
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxOutput)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxOutput)
makeLenses ''TxOutput

data TxInput = MkTxInput
  { _tx_hash :: Hash32
  , _output_index :: Integer
  , _as_output :: TxOutput
  , _redeemer :: Maybe Redeemer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxInput)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxInput)
makeLenses ''TxInput

data TxWitnesses = MkTxWitnesses
  { _vkeywitness :: [Aeson.Value]
  , script :: [Aeson.Value]
  , _plutus_datums :: [Aeson.Value]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxWitnesses)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxWitnesses)

makeLenses ''TxWitnesses
makeLensesFor [("script", "txWitnessesScript")] ''Multiasset

data TxCollateral = MkTxCollateral
  { _collateral :: [Aeson.Value]
  , _collateral_return :: TxOutput
  , _total_collateral :: Integer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxCollateral)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxCollateral)
makeLenses ''TxCollateral

data TxValidity = MkTxValidity
  { _start :: Integer
  , _ttl :: Integer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxValidity)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxValidity)
makeLenses ''TxValidity

data TxAuxiliary = MkTxAuxiliary
  { _metadata :: [Aeson.Value]
  , _scripts :: [Aeson.Value]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxAuxiliary)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxAuxiliary)
makeLenses ''TxAuxiliary

arbitraryTx :: Tx
arbitraryTx =
  MkTx
    { _inputs = []
    , _outputs = []
    , _certificates = []
    , _withdrawals = []
    , _mint = []
    , _reference_inputs = []
    , _witnesses =
        MkTxWitnesses
          { _vkeywitness = []
          , script = []
          , _plutus_datums = []
          }
    , collateral =
        MkTxCollateral
          { _collateral = []
          , _collateral_return =
              MkTxOutput
                { _address = MkAddressAsBase64 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
                , _coin = 0
                , _assets = []
                , _datum = Nothing
                , _script = Nothing
                }
          , _total_collateral = 0
          }
    , _fee = 0
    , _validity =
        MkTxValidity
          { _start = 0
          , _ttl = 0
          }
    , _successful = True
    , _auxiliary =
        MkTxAuxiliary
          { _metadata = []
          , _scripts = []
          }
    , _hash = MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
    }

data Tx = MkTx
  { _inputs :: [TxInput]
  , _outputs :: [TxOutput]
  , _certificates :: [Aeson.Value]
  , _withdrawals :: [Aeson.Value]
  , _mint :: [Aeson.Value]
  , _reference_inputs :: [Aeson.Value]
  , _witnesses :: TxWitnesses
  , collateral :: TxCollateral
  , _fee :: Integer
  , _validity :: TxValidity
  , _successful :: Bool
  , _auxiliary :: TxAuxiliary
  , _hash :: Hash32
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Tx)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Tx)
makeLenses ''Tx
makeLensesFor [("collateral", "txCollateral")] ''Tx

-- PlutusData (JSON representation) and other serialisations

encodePlutusData :: P.Data -> PlutusData
encodePlutusData = MkPlutusData . datumToJson

datumToJson :: P.Data -> Aeson.Value
{-# NOINLINE datumToJson #-}
datumToJson =
  \case
    P.Constr n fields ->
      Aeson.object
        [ "constr"
            .= Aeson.object
              [ "tag" .= Aeson.Number (fromInteger n)
              , "any_constructor" .= Aeson.Number 0
              , "fields"
                  .= Aeson.Array
                    (Vec.fromList $ datumToJson <$> fields)
              ]
        ]
    P.Map kvs ->
      Aeson.object
        [ "map"
            .= Aeson.object
              [ "pairs"
                  .= Aeson.Array
                    ( Vec.fromList $
                        kvs <&> \(k, v) ->
                          Aeson.object
                            [ "key" .= datumToJson k
                            , "value" .= datumToJson v
                            ]
                    )
              ]
        ]
    P.I n ->
      Aeson.object
        [ "big_int"
            .= Aeson.object
              [ "big_n_int"
                  .= Aeson.String
                    ( B64.extractBase64 $
                        B64.encodeBase64 $
                          BS.pack $
                            fromInteger
                              <$> digits @Integer @Double 16 n
                    )
              ]
        ]
    P.B bs ->
      Aeson.object
        [ "bounded_bytes"
            .= Aeson.String
              ( B64.extractBase64 $
                  B64.encodeBase64 bs
              )
        ]
    P.List xs ->
      Aeson.object
        [ "array"
            .= Aeson.object
              [ "items" .= Aeson.Array (datumToJson <$> Vec.fromList xs)
              ]
        ]

digits :: forall n m. (Integral n, RealFrac m, Floating m) => n -> n -> [n]
digits base n =
  fst <$> case reverse [0 .. totalDigits @n @m base n - 1] of
    (i : is) ->
      scanl
        (\(_, remainder) digit -> remainder `divMod` (base ^ digit))
        (n `divMod` (base ^ i))
        is
    [] -> []

totalDigits :: forall n m. (Integral n, RealFrac m, Floating m) => n -> n -> n
totalDigits base = round @m . logBase (fromIntegral base) . fromIntegral

serialisePubKeyHash :: P.PubKeyHash -> Hash28
serialisePubKeyHash = MkBlake2b244Hex . serialiseAsHex . P.getPubKeyHash

serialiseCurrencySymbol :: P.CurrencySymbol -> Hash28
serialiseCurrencySymbol = MkBlake2b244Hex . serialiseAsHex . P.unCurrencySymbol

serialiseScriptHash :: P.ScriptHash -> Hash28
serialiseScriptHash = MkBlake2b244Hex . serialiseAsHex . P.getScriptHash

serialiseTxHash :: P.TxId -> Hash32
serialiseTxHash = MkBlake2b255Hex . serialiseAsHex . P.getTxId

serialiseAsHex :: P.BuiltinByteString -> T.Text
serialiseAsHex =
  B16.extractBase16
    . B16.encodeBase16
    . P.fromBuiltin

plutusAddressToOuraAddress :: (HasCallStack) => P.Address -> Address
plutusAddressToOuraAddress =
  MkAddressAsBase64
    . B64.extractBase64
    . B64.encodeBase64
    . C.serialiseToRawBytes
    . either error id
    . plutusAddressToShelleyAddress L.Mainnet

--------------------------------------------------------------------------------
-- CEM (cardano-api) -> Tx

-- For testing: build a tx in the Oura format from a Cardano tx.
-- We populate only fields we use, use with cautious.
resolvedTxToOura :: C.TxBodyContent C.BuildTx Era -> C.UTxO Era -> Tx
resolvedTxToOura tbc utxo =
  arbitraryTx
    { _inputs = oInputs
    , _outputs = oOutputs
    }
  where
    oInputs = mapMaybe (toOuraInput utxo . fst) (C.txIns tbc)
    oOutputs = toOuraTxOutput <$> C.txOuts tbc

-- | This is a partial function, use with cautious
toOuraInput :: C.UTxO Era -> C.TxIn -> Maybe TxInput
toOuraInput (C.UTxO utxo) txIn =
  case Map.lookup txIn utxo of
    Nothing -> Nothing
    Just output ->
      pure $
        MkTxInput
          { _tx_hash = MkBlake2b255Hex ""
          , _output_index = 0
          , _as_output = toOuraTxOutput output
          , _redeemer = Nothing
          }

-- | This is a partial function, we use address and datum
toOuraTxOutput :: C.TxOut ctx Era -> TxOutput
toOuraTxOutput (C.TxOut addr _ dat _) =
  MkTxOutput
    { _address = toOuraAddrress addr
    , _coin = 0
    , _assets = []
    , _datum = toOuraDatum dat
    , _script = Nothing
    }

-- | This is a partial function, we use only original_cbor.
toOuraDatum :: C.TxOutDatum ctx Era -> Maybe Datum
toOuraDatum = \case
  (C.TxOutDatumInline _ hsd) ->
    let bs = C.serialiseToCBOR hsd
     in Just $
          MkDatum
            { _payload = MkPlutusData Aeson.Null
            , hash = MkBlake2b255Hex ""
            , _original_cbor =
                B16.extractBase16 $ B16.encodeBase16 bs
                -- Base64.extractBase64 $ Base64.encodeBase64 bs
            }
  _ -> Nothing

toOuraAddrress :: C.AddressInEra Era -> Address
toOuraAddrress (C.AddressInEra _ addr) =
  case addr of
    C.ByronAddress _ -> error "Encounter Byron address"
    C.ShelleyAddress {} ->
      addr
        & MkAddressAsBase64
          -- TODO: switch to base64, see https://github.com/mlabs-haskell/cem-script/issues/107
          -- . Base64.extractBase64
          -- . Base64.encodeBase64
          . B16.extractBase16
          . B16.encodeBase16
          . C.serialiseToRawBytes
