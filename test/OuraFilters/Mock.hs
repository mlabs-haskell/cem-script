{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module OuraFilters.Mock where

import Control.Lens.TH (makeLenses, makeLensesFor)
import Data.Aeson (KeyValue ((.=)))
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64.Types
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Data.Vector qualified as Vec
import GHC.Generics (Generic (Rep))
import PlutusLedgerApi.V1 qualified
import Utils (digits)
import Prelude

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
newtype Address = MkAddressAsBase64 T.Text
  deriving newtype (Aeson.ToJSON)
  deriving newtype (Aeson.FromJSON)
makeLenses ''Address

newtype Hash32 = Mk32BitBase16Hash T.Text
  deriving newtype (Aeson.ToJSON)
  deriving newtype (Aeson.FromJSON)
makeLenses ''Hash32

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
  { _policy_id :: T.Text
  , assets :: [Asset]
  , redeemer :: Maybe Aeson.Value
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Multiasset)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Multiasset)
makeLenses ''Multiasset
makeLensesFor
  [ ("assets", "multiassetAssets")
  , ("redeemer", "multiassetRedeemer")
  ]
  ''Multiasset

newtype PlutusData = MkPlutusData {_plutusData :: Aeson.Value}
  deriving newtype (Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)
makeLenses ''PlutusData

data Datum = MkDatum
  { hash :: Hash32
  , _payload :: Maybe PlutusData
  , _original_cbor :: T.Text
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Datum)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Datum)
makeLenses ''Datum
makeLensesFor [("hash", "datum_hash")] ''Datum

data TxOutput = MkTxOutput
  { _address :: Address
  , _coin :: Integer
  , _assets :: [Multiasset]
  , _datum :: Maybe PlutusData
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
  , _redeemer :: Maybe Datum
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxInput)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxInput)
makeLenses ''TxInput

data TxWitnesses = MkTxWitnesses
  { _vkeywitness :: [Aeson.Value]
  , _script :: [Aeson.Value]
  , _plutus_datums :: [Datum]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxWitnesses)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxWitnesses)

-- makeLenses ''TxWitnesses

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
          , _script = []
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
    , _hash = Mk32BitBase16Hash "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
    }

-- Source: https://docs.rs/utxorpc-spec/latest/utxorpc_spec/utxorpc/v1alpha/cardano/struct.Tx.html
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

data TxEvent = MkTxEvent
  { _parsed_tx :: Tx
  , _point :: String -- "Origin"
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxEvent)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxEvent)
makeLenses ''TxEvent

mkTxEvent :: Tx -> TxEvent
mkTxEvent _parsed_tx =
  MkTxEvent
    { _parsed_tx
    , _point = "Origin"
    }

txToText :: TxEvent -> T.Text
txToText =
  T.Encoding.decodeUtf8
    . LBS.toStrict
    . Aeson.encode

encodePlutusData :: PlutusLedgerApi.V1.Data -> PlutusData
encodePlutusData = MkPlutusData . datumToJson

datumToJson :: PlutusLedgerApi.V1.Data -> Aeson.Value
{-# NOINLINE datumToJson #-}
datumToJson =
  \case
    PlutusLedgerApi.V1.Constr n fields ->
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
    PlutusLedgerApi.V1.Map kvs ->
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
    PlutusLedgerApi.V1.I n ->
      Aeson.object
        [ "big_int"
            .= Aeson.object
              [ "big_n_int"
                  .= Aeson.String
                    ( Base64.Types.extractBase64 $
                        Base64.encodeBase64 $
                          BS.pack $
                            fromInteger
                              <$> digits @Integer @Double 16 n
                    )
              ]
        ]
    PlutusLedgerApi.V1.B bs ->
      Aeson.object
        [ "bounded_bytes"
            .= Aeson.String
              ( Base64.Types.extractBase64 $
                  Base64.encodeBase64 bs
              )
        ]
    PlutusLedgerApi.V1.List xs ->
      Aeson.object
        [ "array"
            .= Aeson.object
              [ "items" .= Aeson.Array (datumToJson <$> Vec.fromList xs)
              ]
        ]
