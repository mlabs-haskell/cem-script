-- FIXME
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.CEM.Examples.Compilation where

import Prelude

import Cardano.CEM.Examples.Auction (SimpleAuction)
import Cardano.CEM.Examples.Voting (SimpleVoting)
import Cardano.CEM.TH (compileCEM)

$(compileCEM True ''SimpleAuction)
$(compileCEM False ''SimpleVoting)
