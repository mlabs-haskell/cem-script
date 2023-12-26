module Cardano.CEM.OnChain where

import PlutusTx.Prelude

import Data.Proxy

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V2.Contexts (
  ScriptContext,
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  findOwnInput,
  scriptContextTxInfo,
 )

import Cardano.CEM

data CEMScriptDatum script = MkCEMScriptDatum
  { stageParams :: StageParams (Stage script)
  , params :: Params script
  , state :: State script
  }

class (CEMScript script) => CEMScriptCompiled script where
  cemScriptCompiled :: Proxy script -> SerialisedScript

cemScriptAddress :: (CEMScriptCompiled script) => Proxy script -> Address
cemScriptAddress _ = traceError "TODO"

genericCEMScript ::
  forall script.
  (CEMScript script) =>
  CEMScriptDatum script ->
  Transition script ->
  ScriptContext ->
  Bool
genericCEMScript datum transition context =
  case transitionSpec (params datum) (state datum) transition of
    Right spec ->
      -- do transition
      (and $ map (checkConstraint ownAddress datum info) (сonstraints spec))
        -- check signers
        && ( traceIfFalse "Wrong signers list"
              $ (signers spec)
              `isSubSetOf` txInfoSignatories info
           )
        -- check stage
        && let
            expectedInterval =
              stageToOnChainInterval (stageParams datum) (stage spec)
            in
            traceIfFalse "Wrong interval for transition stage"
              $ expectedInterval
              `contains` txInfoValidRange info
    Left _ -> traceError "TODO"
  where
    info = scriptContextTxInfo context
    ownAddress = case findOwnInput context of
      Just x -> txOutAddress $ txInInfoResolved x
      Nothing -> traceError "Impossible happened"

checkConstraint ::
  Address -> CEMScriptDatum script -> TxInfo -> TxFanConstraint script -> Bool
checkConstraint ownAddress ownDatum info (MkTxFanC fanKind filterSpec quantifier) =
  checkQuantifier $ filter (predFan filterSpec) fans
  where
    fans = case fanKind of
      In -> map txInInfoResolved $ txInfoInputs info
      InRef -> map txInInfoResolved $ txInfoReferenceInputs info
      Out -> txInfoOutputs info
    predFan filterSpec' fan = case filterSpec' of
      Anything -> True
      ByAddress address -> txOutAddress fan == address
      BySameCEM state ->
        let
          stateChangeDatum = ownDatum {state = state}
          stateChangeDatumBS = traceError "TODO"
          cemChangeConstraint =
            And [ByAddress ownAddress, ByDatum stateChangeDatumBS]
         in
          predFan cemChangeConstraint fan
      ByPubKey pubKey -> predFan (ByAddress $ pubKeyHashAddress pubKey) fan
      ByDatum datum -> retrieveFanDatum fan == Just datum
      And subSpecs -> and $ predOnSubSpecs subSpecs
      Or subSpecs -> or $ predOnSubSpecs subSpecs
      where
        predOnSubSpecs = map (flip predFan fan)
    checkQuantifier txFans = case quantifier of
      SumValueEq value -> (foldMap txOutValue txFans) == value
      -- TODO: use natural numbers
      Exist n -> length txFans == n
    retrieveFanDatum fan = traceError "TODO"

isSubSetOf :: (Eq a) => [a] -> [a] -> Bool
isSubSetOf xs ys = and $ map (`elem` ys) xs
