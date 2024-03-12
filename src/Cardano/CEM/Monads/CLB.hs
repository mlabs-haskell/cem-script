{-# LANGUAGE RecordWildCards #-}

module Cardano.CEM.Monads.CLB where

import Prelude

import Data.Set qualified as Set
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
    fail = error "FIXME"

instance MonadBlockchainParams Clb where
    askNetworkId :: Clb NetworkId
    askNetworkId = gets (mockConfigNetworkId . mockConfig)

    queryCurrentSlot :: Clb SlotNo
    queryCurrentSlot = getCurrentSlot

    queryBlockchainParams = do
        -- protocolParameters <- gets (mockConfigProtocol . mockConfig)
        slotConfig <- gets (mockConfigSlotConfig . mockConfig)
        return $ MkBlockchainParams {
            protocolParameters = error "TODO",
            systemStart = error "TODO",
            eraHistory = error "TODO",
            stakePools = Set.empty
        }
