{-# LANGUAGE NoPolyKinds #-}

module Cardano.CEM.Stages where

import PlutusTx qualified

import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  always,
 )

-- Stages

-- This covers constraints on blockchain slot time,
-- used by both on- and off-chain code
class Stages stage where
  type StageParams stage = params | params -> stage
  stageToOnChainInterval ::
    StageParams stage -> stage -> Interval POSIXTime

-- Common

-- TODO: rename
data SingleStage = Always

data SingleStageParams
  = NoSingleStageParams
  | AllowedInterval (Interval POSIXTime)

instance Stages SingleStage where
  type StageParams SingleStage = SingleStageParams

  stageToOnChainInterval NoSingleStageParams Always = always
  stageToOnChainInterval (AllowedInterval interval) Always = interval

PlutusTx.unstableMakeIsData ''SingleStage
PlutusTx.unstableMakeIsData 'NoSingleStageParams
