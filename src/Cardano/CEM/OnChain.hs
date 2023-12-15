{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoPolyKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Cardano.CEM.OnChain where

import PlutusTx.Prelude

import Data.Proxy

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Address (Address, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (always, contains)
import PlutusLedgerApi.V1.Scripts (Datum (..))
import PlutusLedgerApi.V1.Value (geq)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext,
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  findOwnInput,
  scriptContextTxInfo,
 )
import PlutusLedgerApi.V2.Tx (OutputDatum (..))
import PlutusTx.IsData (FromData, ToData (toBuiltinData), UnsafeFromData (..))
import PlutusTx.Show (Show (..))

import Plutus.Extras

import Cardano.CEM
import Cardano.CEM.Examples.Auction
import Cardano.CEM.Stages
import Cardano.Ledger.Babbage.TxBody (getEitherAddrBabbageTxOut)
import Language.Haskell.TH (Code, conT, unsafe)
import Language.Haskell.TH.Syntax (Dec, Exp, Name, Q, Type)

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
-- Typed quasiquotes do not allow type splicing, so we need use untyped
-- Fields bug - https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8686
-- Data famlily - not suported -
-- https://github.com/IntersectMBO/plutus/issues/5768
-- Type familiy mentioning: https://github.com/IntersectMBO/plutus/issues/5769

{-# INLINEABLE genericCEMScript #-}
genericCEMScript ::
  Name ->
  Name ->
  Q Exp
genericCEMScript script scriptStage =
  [|
    \datum' redeemer' context' ->
      let
        checkTxFan' ownDatum filterSpec' fan =
          case filterSpec' of
            Anything -> True
            UnsafeBySameCEM stateData ->
              let
                state = unsafeFromBuiltinData stateData :: State $(conT script)
                (p1, p2, _) = ownDatum
                stateChangeDatum = (p1, p2, state)
                stateChangeDatumBS = toBuiltinData stateChangeDatum
               in
                checkTxFan' ownDatum (ByDatum stateChangeDatumBS) fan
            ByDatum expecedDatum ->
              let
                TxOut _ _ datum _ = fan
               in
                case datum of
                  OutputDatum datum -> getDatum datum == expecedDatum
                  OutputDatumHash _ -> traceError "Hash datum not supported"
                  _ -> False
        checkConstraint ownDatum ownAddress info (MkTxFanC fanKind filterSpec quantifier) =
          traceIfFalse ("Checking constraint " <> show fanKind <> " " <> show datumSpec)
            $ checkQuantifier
            $ filter checkTxFan fans
          where
            MkTxFanFilter addressSpec datumSpec = filterSpec
            checkTxFan fan =
              checkTxFanAddress ownAddress addressSpec fan
                && checkTxFan' ownDatum datumSpec fan
            fans = case fanKind of
              In -> map txInInfoResolved $ txInfoInputs info
              InRef -> map txInInfoResolved $ txInfoReferenceInputs info
              Out -> txInfoOutputs info
            checkQuantifier txFans =
              case quantifier of
                SumValueEq value ->
                  foldMap txOutValue txFans `geq` value
                Exist n -> length txFans == n

        params :: Params $(conT script)
        stageParams :: StageParams ($(conT scriptStage))
        datum :: CEMScriptDatum $(conT script)
        datum = unsafeFromBuiltinData datum'
        (stageParams, params, state) = datum
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
                ( all (checkConstraint datum ownAddress info) constraints
                )
                -- check signers
                && traceIfFalse
                  "Wrong signers list"
                  ( signers
                      `isSubSetOf` txInfoSignatories info
                  )
                -- check stage
                && let
                    expectedInterval =
                      always
                      -- stageToOnChainInterval' stageParams (traceError "TODO")
                    in
                    traceIfFalse "Wrong interval for transition stage"
                      $ expectedInterval
                      `contains` txInfoValidRange info
            Left _ -> traceIfFalse "Wrong transition" False
       in
        if True
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
