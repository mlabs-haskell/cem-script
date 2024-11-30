{- FIXME: remove -}
{-# LANGUAGE BlockArguments #-}

module Oura.Config (
  -- filtersL,
  -- predicateL,
  -- tableL,
  -- atKey,
  -- _Table,
  -- _Integer,
  -- _Bool,
  -- _Text,
) where

-- import Prelude

-- import Cardano.CEM.Indexing qualified as Config
-- import Control.Lens (
--   At (at),
--   Each (each),
--   Iso',
--   Lens',
--   Prism',
--   Traversal',
--   from,
--   iso,
--   mapping,
--   partsOf,
--   prism',
--   _Just,
--  )
-- import Data.Map (Map)
-- import Data.Text qualified as T
-- import Toml qualified

-- -- * Config

-- filterL :: Iso' Config.Filter Toml.Table
-- filterL = iso Config.unFilter Config.MkFilter

-- predicateL :: Traversal' Config.Filter T.Text
-- predicateL = filterL . atKey "predicate" . _Just . _Text

-- filtersL :: Traversal' Toml.Table [Config.Filter]
-- filtersL =
--   atKey "filters"
--     . _Just
--     . _List
--     . partsOf (each . _Table . from filterL)

-- atKey :: T.Text -> Traversal' Toml.Table (Maybe Toml.Value)
-- atKey key = tableL . at key

-- tableL :: Lens' Toml.Table (Map T.Text Toml.Value)
-- tableL =
--   iso (\(Toml.MkTable t) -> t) Toml.MkTable
--     . mapping (iso snd ((),))

-- _Table :: Prism' Toml.Value Toml.Table
-- _Table = prism' Toml.Table \case
--   Toml.Table table -> Just table
--   _ -> Nothing

-- _Text :: Prism' Toml.Value T.Text
-- _Text = prism' Toml.Text \case
--   Toml.Text t -> Just t
--   _ -> Nothing

-- _List :: Prism' Toml.Value [Toml.Value]
-- _List = prism' Toml.List \case
--   Toml.List xs -> Just xs
--   _ -> Nothing

-- _Bool :: Prism' Toml.Value Bool
-- _Bool = prism' Toml.Bool \case
--   Toml.Bool b -> Just b
--   _ -> Nothing

-- _Integer :: Prism' Toml.Value Integer
-- _Integer = prism' Toml.Integer \case
--   Toml.Integer n -> Just n
--   _ -> Nothing
