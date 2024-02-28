{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.CEM.Examples.Compilation where

import PlutusTx qualified

import Data.Proxy (Proxy (..))

import PlutusLedgerApi.V2 (ScriptContext (ScriptContext), serialiseCompiledCode)

import Cardano.Api (PlutusScript)
import Cardano.CEM
import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Voting
import Cardano.CEM.OnChain (CEMScriptCompiled (..), genericCEMScript)
import Cardano.CEM.Stages (SingleStage)
import Plutus.Extras

compiledAuction = $(PlutusTx.compileUntyped (genericCEMScript ''SimpleAuction ''SimpleAuctionStage))

instance CEMScriptCompiled SimpleAuction where
  {-# INLINEABLE cemScriptCompiled #-}
  cemScriptCompiled Proxy =
    serialiseCompiledCode compiledAuction

compiledVoting = $(PlutusTx.compileUntyped (genericCEMScript ''SimpleVoting ''SingleStage))

instance CEMScriptCompiled SimpleVoting where
  {-# INLINEABLE cemScriptCompiled #-}
  cemScriptCompiled Proxy =
    serialiseCompiledCode compiledVoting
