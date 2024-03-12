{-# LANGUAGE RecordWildCards #-}

module Cardano.CEM.Monads.CLB where

import Prelude

import Text.Show.Pretty

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (gets)
import Control.Monad.Trans (MonadIO (..))

-- Cardano imports
import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (LedgerProtocolParameters (..))

-- Lib imports
import Clb
import Clb.MockConfig

-- CEM imports

import Cardano.CEM
import Cardano.CEM.Monads
import Cardano.Extras

instance MonadFail Clb where
    fail = error "TODO"

instance MonadBlockchainParams Clb where
    askNetworkId :: Clb NetworkId
    askNetworkId = gets (mockConfigNetworkId . mockConfig)

    -- mockConfigProtocolb

    queryCurrentSlot :: Clb SlotNo
    queryCurrentSlot = getCurrentSlot
    --     clbState <- ask
    --     return clbState.ledgerEnv.ledgerSlotNo

