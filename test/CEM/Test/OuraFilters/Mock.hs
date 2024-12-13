{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module CEM.Test.OuraFilters.Mock where

import Cardano.CEM.Indexing.Tx (Tx, WithoutUnderscore (..))
import Control.Lens.TH (makeLenses)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import GHC.Generics (Generic)
import Prelude

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

txToBS :: TxEvent -> BS.ByteString
txToBS = LBS.toStrict . Aeson.encode
