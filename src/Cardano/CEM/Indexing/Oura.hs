{- | CEM provides the building blocks to build an indexer for your dApp.
Current implementation is based on Oura. This module provides tools to
run Oura daemon, most important being function to buil a config for Oura
for a particular CEM Script.
-}
module Cardano.CEM.Indexing.Oura (
  SourcePath (MkSourcePath, unSourcePath),
  SinkPath (MkSinkPath, unSinkPath),
  Filter (MkFilter, unFilter),
  daemonConfig,
  selectByAddress,
  ouraMonitoringScript,
  configToText,

  -- * Addresses
  AddressBech32 (MkAddressBech32, unAddressBech32),
  cardanoAddressBech32,
) where

import Cardano.Api qualified as C
import Cardano.CEM.Address (cemScriptAddress)
import Cardano.CEM.OnChain (CEMScriptCompiled)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.Data (Proxy)
import Data.String (IsString)
import Data.Text qualified as T
import Toml (Table)
import Toml qualified
import Toml.Pretty qualified
import Toml.Schema ((.=))
import Toml.Schema.ToValue qualified as Toml.ToValue
import Prelude

newtype SourcePath = MkSourcePath {unSourcePath :: T.Text}
  deriving newtype (IsString)

newtype SinkPath = MkSinkPath {unSinkPath :: T.Text}
  deriving newtype (IsString)

newtype Filter = MkFilter {unFilter :: Toml.Table}
  deriving newtype (Eq, Show)

daemonConfig :: [Filter] -> SourcePath -> SinkPath -> Toml.Table
daemonConfig filters sourcePath sinkPath =
  Toml.ToValue.table
    [ "filters" .= Toml.List (Toml.Table . unFilter <$> filters)
    , "cursor" .= cursor
    , "intersect" .= intersect
    , "sink" .= sink sinkPath
    , "source" .= source sourcePath
    ]

-- | A oura *filter* that selects by address
selectByAddress :: AddressBech32 -> Filter
selectByAddress (MkAddressBech32 addressBech32) =
  MkFilter $
    Toml.ToValue.table
      [ "predicate" .= Toml.Text addressBech32 -- "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
      , "skip_uncertain" .= Toml.Bool False
      , "type" .= Toml.Text "Select"
      ]

-- | Makes an oura config such that oura is going to monitor all spendings from the script's payment credential.
ouraMonitoringScript ::
  forall script.
  (CEMScriptCompiled script) =>
  Proxy script ->
  Ledger.Network ->
  SourcePath ->
  SinkPath ->
  Either String Toml.Table
ouraMonitoringScript p network sourcePath sinkPath =
  (\filters -> daemonConfig filters sourcePath sinkPath)
    . pure
    . selectByAddress
    . cardanoAddressBech32
    <$> cemScriptAddress network p

cursor :: Toml.Table
cursor =
  Toml.ToValue.table
    [ "path" .= Toml.Text "./oura-daemon-cursor"
    , "type" .= Toml.Text "File"
    ]

intersect :: Toml.Table
intersect =
  Toml.ToValue.table
    [ "type" .= Toml.Text "Point"
    , "value"
        .= Toml.List
          [ Toml.Integer 37225013
          , Toml.Text "65b3d40e6114e05b662ddde737da63bbab05b86d476148614e82cde98462a6f5"
          ]
    ]

sink :: SinkPath -> Toml.Table
sink (MkSinkPath sinkPath) =
  Toml.ToValue.table
    [ "compress_files" .= Toml.Bool True
    , "max_bytes_per_file" .= Toml.Integer 1_000_000
    , "max_total_files" .= Toml.Integer 10
    , "output_format" .= Toml.Text "JSONL"
    , "output_path" .= Toml.Text sinkPath
    , "type" .= Toml.Text "FileRotate"
    ]

source :: SourcePath -> Toml.Table
source (MkSourcePath socketPath) =
  Toml.ToValue.table
    [ "socket_path" .= Toml.Text socketPath
    , "type" .= Toml.Text "TxOverSocket"
    ]

configToText :: Table -> T.Text
configToText = T.pack . show . Toml.Pretty.prettyToml

newtype AddressBech32 = MkAddressBech32 {unAddressBech32 :: T.Text}
  deriving newtype (Eq, Show, IsString)

cardanoAddressBech32 :: C.Address C.ShelleyAddr -> AddressBech32
cardanoAddressBech32 = MkAddressBech32 . C.serialiseToBech32
