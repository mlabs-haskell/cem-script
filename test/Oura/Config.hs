module Oura.Config (
  daemonConfig,
  SourcePath (MkSourcePath, unSourcePath),
  SinkPath (MkSinkPath, unSinkPath),
) where

import Data.String (IsString)
import Data.Text qualified as T
import Toml qualified
import Toml.Schema.ToValue ((.=))
import Toml.Schema.ToValue qualified as Toml.ToValue
import Prelude

-- * Config

newtype SourcePath = MkSourcePath {unSourcePath :: T.Text}
  deriving newtype (IsString)

newtype SinkPath = MkSinkPath {unSinkPath :: T.Text}
  deriving newtype (IsString)

daemonConfig :: SourcePath -> SinkPath -> Toml.Table
daemonConfig sourcePath sinkPath =
  Toml.ToValue.table
    [ "filters" .= Toml.List filters
    , "cursor" .= cursor
    , "intersect" .= intersect
    , "sink" .= sink sinkPath
    , "source" .= source sourcePath
    ]

filters :: [Toml.Value]
filters =
  [ Toml.Table $
      Toml.ToValue.table
        [ "predicate" .= Toml.Text "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x"
        , "skip_uncertain" .= Toml.Bool False
        , "type" .= Toml.Text "Select"
        ]
  ]

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
