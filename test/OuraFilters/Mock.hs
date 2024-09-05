module OuraFilters.Mock where
import Prelude
import Control.Lens.TH (makeLenses)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson

newtype Address = MkBech32AsBase32 { base32bech32addr :: LBS.ByteString }
makeLenses ''Address

newtype Hash32 = Mk32BitBase16Hash { unHash32 :: LBS.ByteString }
makeLenses ''Hash32

data TxOutput = MkTxOutput
  { address :: Address
  , coin :: Integer
  , assets :: [Aeson.Value]
  , datum :: Maybe Aeson.Value
  , datum_hash :: Hash32
  , script :: Maybe Aeson.Value
  }
makeLenses ''TxOutput

data TxInput = MkTxInput
  { tx_hash :: Hash32
  , output_index :: Integer
  , as_output :: TxOutput
  , redeemer :: Maybe Aeson.Value
  }
makeLenses ''TxInput

data TxWitnesses = MkTxWitnesses
  { vkeywitness :: [Aeson.Value]
  , script :: [Aeson.Value]
  , plutus_datums :: [Aeson.Value]
  }
makeLenses ''TxWitnesses

data TxCollateral = MkTxCollateral
  { collateral :: [Aeson.Value]
  , collateral_return :: TxOutput
  , total_collateral :: Integer
  }
makeLenses ''TxCollateral

data TxValidity = MkTxValidity
  { start :: Integer
  , ttl :: Integer
  }
makeLenses ''TxValidity

data TxAuxiliary = MkTxAuxiliary
  { metadata :: [Aeson.Value]
  , scripts :: [Aeson.Value]
  }
makeLenses ''TxAuxiliary

data Tx = MkTx
  { inputs :: [TxInput]
  , outputs :: [TxOutput]
  , certificates :: [Aeson.Value]
  , withdrawals :: [Aeson.Value]
  , mint :: [Aeson.Value]
  , reference_inputs :: [Aeson.Value]
  , witnesses :: TxWitnesses
  , collateral :: TxCollateral
  , fee :: Integer
  , validity :: TxValidity
  , successful :: Bool
  , auxiliary :: TxAuxiliary
  , hash :: Hash32
  }
makeLenses ''Tx

data TxEvent = MkTxEvent
  { parsed_tx :: Tx
  , point :: String -- "Origin"
  }
makeLenses ''TxEvent

mkTxEvent :: Tx -> TxEvent
mkTxEvent parsed_tx = MkTxEvent
  { parsed_tx
  , point = "Origin"
  }