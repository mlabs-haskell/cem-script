{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE DuplicateRecordFields, NoFieldSelectors #-}
module OuraFilters.Mock where
import Prelude
import Control.Lens.TH (makeLenses, makeLensesFor)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import GHC.Generics (Generic (Rep))

newtype WithoutUnderscore a = MkWithoutUnderscore a
  deriving newtype Generic

withoutLeadingUnderscore :: Aeson.Options
withoutLeadingUnderscore = 
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
      '_':fieldName -> fieldName
      fieldName -> fieldName
    }
instance
  (Generic a
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (GHC.Generics.Rep a)
  ) => Aeson.ToJSON (WithoutUnderscore a) where
  toJSON = Aeson.genericToJSON withoutLeadingUnderscore
instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.FromJSON (WithoutUnderscore a) where
  parseJSON = Aeson.genericParseJSON withoutLeadingUnderscore


newtype Address = MkBech32AsBase32 T.Text
  deriving newtype Aeson.ToJSON
  deriving newtype Aeson.FromJSON
makeLenses ''Address

newtype Hash32 = Mk32BitBase16Hash T.Text
  deriving newtype Aeson.ToJSON
  deriving newtype Aeson.FromJSON
makeLenses ''Hash32

data TxOutput = MkTxOutput
  { _address :: Address
  , _coin :: Integer
  , _assets :: [Aeson.Value]
  , _datum :: Maybe Aeson.Value
  , _datum_hash :: Hash32
  , _script :: Maybe Aeson.Value
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxOutput)
  deriving Aeson.FromJSON via (WithoutUnderscore TxOutput)
makeLenses ''TxOutput

data TxInput = MkTxInput
  { _tx_hash :: Hash32
  , _output_index :: Integer
  , _as_output :: TxOutput
  , _redeemer :: Maybe Aeson.Value
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxInput)
  deriving Aeson.FromJSON via (WithoutUnderscore TxInput)
makeLenses ''TxInput

data TxWitnesses = MkTxWitnesses
  { _vkeywitness :: [Aeson.Value]
  , _script :: [Aeson.Value]
  , _plutus_datums :: [Aeson.Value]
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxWitnesses)
  deriving Aeson.FromJSON via (WithoutUnderscore TxWitnesses)
-- makeLenses ''TxWitnesses

data TxCollateral = MkTxCollateral
  { _collateral :: [Aeson.Value]
  , _collateral_return :: TxOutput
  , _total_collateral :: Integer
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxCollateral)
  deriving Aeson.FromJSON via (WithoutUnderscore TxCollateral)
makeLenses ''TxCollateral

data TxValidity = MkTxValidity
  { _start :: Integer
  , _ttl :: Integer
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxValidity)
  deriving Aeson.FromJSON via (WithoutUnderscore TxValidity)
makeLenses ''TxValidity

data TxAuxiliary = MkTxAuxiliary
  { _metadata :: [Aeson.Value]
  , _scripts :: [Aeson.Value]
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxAuxiliary)
  deriving Aeson.FromJSON via (WithoutUnderscore TxAuxiliary)
makeLenses ''TxAuxiliary

arbitraryTx :: Tx
arbitraryTx = MkTx
  { _inputs = []
  , _outputs = []
  , _certificates = []
  , _withdrawals = []
  , _mint = []
  , _reference_inputs = []
  , _witnesses = MkTxWitnesses
    { _vkeywitness = []
    , _script = []
    , _plutus_datums = []
    }
  , collateral = MkTxCollateral
    { _collateral = []
    , _collateral_return = MkTxOutput
      { _address = MkBech32AsBase32 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
      , _coin = 0
      , _assets = []
      , _datum = Nothing
      , _datum_hash = Mk32BitBase16Hash "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
      , _script = Nothing
      }
    , _total_collateral = 0
    }
  , _fee = 0
  , _validity = MkTxValidity
    { _start = 0
    , _ttl = 0
    }
  , _successful = True
  , _auxiliary = MkTxAuxiliary
    { _metadata = []
    , _scripts = []
    }
  , _hash = Mk32BitBase16Hash "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
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
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore Tx)
  deriving Aeson.FromJSON via (WithoutUnderscore Tx)
makeLenses ''Tx
makeLensesFor [("collateral","collateralL")] ''Tx

data TxEvent = MkTxEvent
  { _parsed_tx :: Tx
  , _point :: String -- "Origin"
  }
  deriving stock Generic
  deriving Aeson.ToJSON via (WithoutUnderscore TxEvent)
  deriving Aeson.FromJSON via (WithoutUnderscore TxEvent)
makeLenses ''TxEvent


mkTxEvent :: Tx -> TxEvent
mkTxEvent _parsed_tx = MkTxEvent
  { _parsed_tx
  , _point = "Origin"
  }


txToText :: TxEvent -> T.Text
txToText = T.Encoding.decodeUtf8
  . LBS.toStrict
  . Aeson.encode