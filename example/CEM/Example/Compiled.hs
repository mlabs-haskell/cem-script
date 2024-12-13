{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module CEM.Example.Compiled where

import CEM.Example.Auction (SimpleAuction)
import CEM.Example.Voting (SimpleVoting)
import Cardano.CEM
import Prelude

$(compileCEM True ''SimpleAuction)
$(compileCEM False ''SimpleVoting)
