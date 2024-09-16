{-# LANGUAGE BlockArguments #-}

module Oura.Config (
  daemonConfig,
  SourcePath (MkSourcePath, unSourcePath),
  SinkPath (MkSinkPath, unSinkPath),
  filtersL,
  predicateL,
  tableL,
  atKey,
  _Table,
  _Integer,
  _Bool,
  _Text,
) where

import Prelude

import Control.Lens (
  At (at),
  Each (each),
  Iso',
  Lens',
  Prism',
  Traversal',
  from,
  iso,
  mapping,
  partsOf,
  prism',
  _Just,
 )
import Data.Map (Map)
import Data.String (IsString)
import Data.Text qualified as T
import Toml qualified
import Toml.Schema.ToValue ((.=))
import Toml.Schema.ToValue qualified as Toml.ToValue

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

newtype Filter = MkFilter {unFilter :: Toml.Table}
  deriving newtype (Eq, Show)

filterL :: Iso' Filter Toml.Table
filterL = iso unFilter MkFilter

predicateL :: Traversal' Filter T.Text
predicateL = filterL . atKey "predicate" . _Just . _Text

filtersL :: Traversal' Toml.Table [Filter]
filtersL =
  atKey "filters"
    . _Just
    . _List
    . partsOf (each . _Table . from filterL)

atKey :: T.Text -> Traversal' Toml.Table (Maybe Toml.Value)
atKey key = tableL . at key

tableL :: Lens' Toml.Table (Map T.Text Toml.Value)
tableL =
  iso (\(Toml.MkTable t) -> t) Toml.MkTable
    . mapping (iso snd ((),))

_Table :: Prism' Toml.Value Toml.Table
_Table = prism' Toml.Table \case
  Toml.Table table -> Just table
  _ -> Nothing

_Text :: Prism' Toml.Value T.Text
_Text = prism' Toml.Text \case
  Toml.Text t -> Just t
  _ -> Nothing

_List :: Prism' Toml.Value [Toml.Value]
_List = prism' Toml.List \case
  Toml.List xs -> Just xs
  _ -> Nothing

_Bool :: Prism' Toml.Value Bool
_Bool = prism' Toml.Bool \case
  Toml.Bool b -> Just b
  _ -> Nothing

_Integer :: Prism' Toml.Value Integer
_Integer = prism' Toml.Integer \case
  Toml.Integer n -> Just n
  _ -> Nothing
