{-# LANGUAGE NoPolyKinds #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

module Cardano.CEM.OnChain (
  CEMScriptCompiled (..),
  cemScriptAddress,
  genericCEMScript,
) where

import PlutusTx.Prelude

import Data.Proxy (Proxy)
import Language.Haskell.TH (conT)
import Language.Haskell.TH.Syntax (Exp, Name, Q)

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Address (Address, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (always, contains)
import PlutusLedgerApi.V1.Scripts (Datum (..))
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V2.Contexts (
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  findOwnInput,
  scriptContextTxInfo,
 )
import PlutusLedgerApi.V2.Tx (OutputDatum (..))
import PlutusTx.IsData (FromData, ToData (toBuiltinData), UnsafeFromData (..))
import PlutusTx.Show (Show (..))

import Cardano.CEM
import Cardano.CEM.Stages
import Plutus.Extras (scriptValidatorHash)

class (CEMScript script, CEMScriptIsData script) => CEMScriptCompiled script where
  cemScriptCompiled :: Proxy script -> SerialisedScript

{-# INLINEABLE cemScriptAddress #-}
cemScriptAddress ::
  forall script. (CEMScriptCompiled script) => Proxy script -> Address
cemScriptAddress =
  scriptHashAddress . scriptValidatorHash . cemScriptCompiled

type IsData x = (UnsafeFromData x, FromData x, ToData x)

type CEMScriptIsData script =
  ( UnsafeFromData (Transition script)
  , IsData (StageParams (Stage script))
  , IsData (Params script)
  , IsData (Transition script)
  , IsData (State script)
  )

-- Various hacks and type annotations are done due to Plutus limitations
-- Typed quasi-quotes do not allow type splicing, so we need use untyped
-- Fields bug - https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8686
-- Data family - not supported -
-- https://github.com/IntersectMBO/plutus/issues/5768
-- Type family mentioning: https://github.com/IntersectMBO/plutus/issues/5769

{-# INLINEABLE genericCEMScript #-}
genericCEMScript ::
  Name ->
  Name ->
  Q Exp
genericCEMScript script scriptStage =
  [|
    \datum' redeemer' context' ->
      let
        checkTxFan' filterSpec' fan =
          case filterSpec' of
            AnyDatum -> True
            UnsafeBySameCEM stateData ->
              let
                -- FIXUP: do not decode unnecessary
                changedState =
                  unsafeFromBuiltinData stateData :: State $(conT script)
                stateChangeDatum = (stageParams, params, stateData)
                stateChangeDatumBS = toBuiltinData stateChangeDatum
               in
                checkTxFan' (ByDatum stateChangeDatumBS) fan
            ByDatum expectedDatum ->
              let
                TxOut _ _ datum _ = fan
               in
                case datum of
                  OutputDatum datumContent ->
                    getDatum datumContent == expectedDatum
                  OutputDatumHash _ -> traceError "Hash datum not supported"
                  _ -> False
        -- given a fans constraint checks it against all fans
        checkConstraint (MkTxFansC fanKind filterSpec quantifier) =
          traceIfFalse ("Checking constraint " <> show fanKind <> " " <> show datumSpec)
            $ checkQuantifier
            $ filter checkTxFan fans
          where
            MkTxFanFilter addressSpec datumSpec = filterSpec
            checkTxFan fan =
              checkTxFanAddress ownAddress addressSpec fan
                && checkTxFan' datumSpec fan
            fans = case fanKind of
              In -> map txInInfoResolved $ txInfoInputs info
              InRef -> map txInInfoResolved $ txInfoReferenceInputs info
              Out -> txInfoOutputs info
            checkQuantifier txFans =
              case quantifier of
                FansWithTotalValueOfAtLeast value ->
                  foldMap txOutValue txFans `geq` value
                ExactlyNFans n -> length txFans == n

        params :: Params $(conT script)
        stageParams :: StageParams ($(conT scriptStage))
        ownDatum :: CEMScriptDatum $(conT script)
        ownDatum = unsafeFromBuiltinData datum'
        (stageParams, params, state) = ownDatum
        transition :: Transition $(conT script)
        transition = unsafeFromBuiltinData redeemer'
        context = unsafeFromBuiltinData context'
        info = scriptContextTxInfo context
        ownAddress = case findOwnInput context of
          Just x -> txOutAddress $ txInInfoResolved x
          Nothing -> traceError "Impossible happened"
        transitionSpec' = transitionSpec @($(conT script))
        stageToOnChainInterval' = stageToOnChainInterval @($(conT scriptStage))
        result =
          case transitionSpec' params (Just state) transition of
            Right (MkTransitionSpec @($(conT script)) constraints signers) ->
              -- do transition
              traceIfFalse
                "Some constraint not matching"
                (all checkConstraint constraints)
                -- check signers
                && traceIfFalse
                  "Wrong signers list"
                  (signers `isSubSetOf` txInfoSignatories info)
                -- check stage
                && let
                    expectedInterval =
                      always
                    in
                    -- stageToOnChainInterval' stageParams (traceError "TODO")

                    traceIfFalse "Wrong interval for transition stage"
                      $ expectedInterval
                      `contains` txInfoValidRange info
            Left _ -> traceIfFalse "Wrong transition" False
       in
        if result
          then ()
          else error ()
    |]

{-# INLINEABLE checkTxFanAddress #-}
checkTxFanAddress :: Address -> AddressSpec -> TxOut -> Bool
checkTxFanAddress ownAddress addressSpec fan =
  txOutAddress fan == addressSpecToAddress ownAddress addressSpec

{-# INLINEABLE isSubSetOf #-}
isSubSetOf :: (Eq a) => [a] -> [a] -> Bool
isSubSetOf xs ys = all (`elem` ys) xs
