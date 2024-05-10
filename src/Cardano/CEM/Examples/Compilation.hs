{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.CEM.Examples.Compilation where

import PlutusTx qualified

import Data.Proxy (Proxy (..))

import PlutusLedgerApi.V2 (serialiseCompiledCode)

import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Voting
import Cardano.CEM.OnChain (CEMScriptCompiled (..), genericCEMScript)
import Cardano.CEM.Stages (SingleStage)

instance CEMScriptCompiled SimpleAuction where
  {-# INLINEABLE cemScriptCompiled #-}
  cemScriptCompiled Proxy =
    serialiseCompiledCode
      $(PlutusTx.compileUntyped (genericCEMScript ''SimpleAuction ''SimpleAuctionStage))

instance CEMScriptCompiled SimpleVoting where
  {-# INLINEABLE cemScriptCompiled #-}
  cemScriptCompiled Proxy =
    serialiseCompiledCode
      $(PlutusTx.compileUntyped (genericCEMScript ''SimpleVoting ''SingleStage))
