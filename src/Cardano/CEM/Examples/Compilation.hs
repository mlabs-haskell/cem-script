{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.CEM.Examples.Compilation where

import PlutusLedgerApi.V2 (serialiseCompiledCode)

import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Voting
import Cardano.CEM.TH

$(compileCEM ''SimpleAuction)
$(compileCEM ''SimpleVoting)
