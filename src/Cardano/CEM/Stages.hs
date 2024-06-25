{-# LANGUAGE NoPolyKinds #-}

module Cardano.CEM.Stages where

import PlutusTx qualified
import Prelude qualified

import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  always,
 )

-- Stages

-- This covers constraints on blockchain slot time,
-- used by both on- and off-chain code
class Stages stage params | params -> stage where
  type StageParams stage
  stageToOnChainInterval ::
    params -> stage -> Interval POSIXTime

-- Common

-- TODO: rename
data SingleStage = Always
  deriving stock (Prelude.Show, Prelude.Eq)

data SingleStageParams
  = NoSingleStageParams
  | AllowedInterval (Interval POSIXTime)
  deriving stock (Prelude.Show, Prelude.Eq)

instance Stages SingleStage SingleStageParams where
  type StageParams SingleStage = SingleStageParams

  stageToOnChainInterval NoSingleStageParams Always = always
  stageToOnChainInterval (AllowedInterval interval) Always = interval

PlutusTx.unstableMakeIsData ''SingleStage
PlutusTx.unstableMakeIsData 'NoSingleStageParams
