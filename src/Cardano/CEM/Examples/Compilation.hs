{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.CEM.Examples.Compilation where

import Prelude

import Data.Set qualified as Set

import PlutusLedgerApi.V2 (BuiltinByteString, CurrencySymbol (CurrencySymbol), PubKeyHash, serialiseCompiledCode)

import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Voting
import Cardano.CEM.TH
import PlutusTx.Blueprint

$(compileCEM ''SimpleAuction)
$(compileCEM ''SimpleVoting)

-- definitions :: Definitions (Unroll Value)
-- definitions = error "TODO"
-- deriveDefinitions @'[[CurrencySymbol]]

type RefTypes = '[BuiltinByteString, PubKeyHash]

votingBlueprint :: ContractBlueprint
votingBlueprint =
  MkContractBlueprint
    { contractId = Just "voting"
    , contractPreamble = votingPreamble
    , contractValidators = Set.fromList [votingValidator]
    , -- cemScriptCompiled $ Proxy @SimpleVoting
      contractDefinitions = deriveDefinitions @'[SimpleVotingTransition]
    }
  where
    votingPreamble =
      MkPreamble
        { preambleTitle = "Voting DApp"
        , preambleDescription = Nothing
        , preambleVersion = "0.1"
        , preamblePlutusVersion = PlutusV2
        , preambleLicense = Nothing
        }
    votingValidator =
      MkValidatorBlueprint
        { validatorTitle = "My Validator"
        , validatorDescription = Nothing
        , validatorParameters = []
        , validatorRedeemer =
            MkArgumentBlueprint
              { argumentTitle = Nothing
              , argumentDescription = Nothing
              , argumentPurpose = Set.singleton Spend
              , argumentSchema = definitionRef @SimpleVotingTransition
              }
        , validatorDatum =
            Just
              MkArgumentBlueprint
                { argumentTitle = Nothing
                , argumentDescription = Nothing
                , argumentPurpose = Set.singleton Spend
                , argumentSchema = definitionRef @SimpleVotingState
                }
        , validatorCompiledCode = Nothing
        }
