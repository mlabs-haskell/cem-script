{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

module Cardano.CEM.OnChain (
  CEMScriptCompiled (..),
  cemScriptAddress,
  genericPlutarchScript,
) where

import Prelude

import Data.Proxy (Proxy (..))
import Language.Haskell.TH (conT)
import Language.Haskell.TH.Syntax (Exp, Name, Q)

import PlutusLedgerApi.V1.Address (Address, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (always, contains)
import PlutusTx qualified
import PlutusTx.IsData.Class

import Data.Singletons
import Plutarch
import Plutarch.FFI (foreignImport)
import Plutarch.LedgerApi
import Plutarch.LedgerApi.Interval (palways, pcontains)
import Plutarch.List
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Cardano.CEM
import Cardano.CEM.Stages (
  Stages (StageParams, stageToOnChainInterval),
 )
import Data.Map qualified as Map
import Data.Spine
import Data.String (IsString (..))
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Maybe (pfromJust)
import Plutarch.Script (serialiseScript)
import Plutarch.Unsafe (punsafeCoerce)
import Plutus.Extras (scriptValidatorHash)
import PlutusLedgerApi.V2 (Interval, POSIXTime)

class (CEMScript script, CEMScriptIsData script) => CEMScriptCompiled script where
  cemScriptCompiled :: Proxy script -> Script

{-# INLINEABLE cemScriptAddress #-}
cemScriptAddress ::
  forall script. (CEMScriptCompiled script) => Proxy script -> Address
cemScriptAddress =
  scriptHashAddress . scriptValidatorHash . serialiseScript . cemScriptCompiled

type IsData x = (UnsafeFromData x, FromData x, ToData x)

type CEMScriptIsData script =
  ( UnsafeFromData (Transition script)
  , IsData (Stage script)
  , IsData (StageParams (Stage script))
  , IsData (Params script)
  , IsData (Transition script)
  , IsData (State script)
  )

genericPlutarchScript :: Name -> Name -> Q Exp
genericPlutarchScript scriptName stageName =
  [|
    plutarchScript
      scriptName
      (Proxy :: Proxy $(conT scriptName))
      (Proxy :: Proxy $(conT stageName))
    |]

--   ( $( PlutusTx.compileUntyped [|\p s t -> traceError "ToDO"|]
--         -- [|
--         --   ( \p s t ->
--         --       PlutusTx.toBuiltinData
--         --         $ case transitionSpec @($(conT scriptName)) (unsafeFromBuiltinData p) (unsafeFromBuiltinData s) (unsafeFromBuiltinData t) of
--         --           Right spec -> spec
--         --   )
--         --   |]
--      )
--   )
-- \|]

-- compiled :: CEMScript script => Proxy script
--               -> PlutusTx.CompiledCode
--                    (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData)
-- compiled (Proxy :: Proxy script) = )

plutarchScript ::
  forall script stage s.
  (CEMScript script, CEMScriptIsData script, stage Prelude.~ Stage script, ToData (TransitionSpec script)) =>
  Name ->
  Proxy script ->
  Proxy stage ->
  -- ((PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData))) ->
  Term s (PData :--> PData :--> PAsData PScriptContext :--> PUnit)
plutarchScript scriptName proxy proxyStage =
  plam $ \datum redm ctx' -> P.do
    ctx <- pletFields @'["txInfo"] ctx'
    purpose <- pmatch $ pfromData $ pfield @"purpose" # ctx'
    let outref' = case purpose of
          PSpending outRef' -> pfield @"_0" # outRef'
          _ -> perror
    let ownAddress =
          pfield @"address"
            #$ pfield @"resolved"
            #$ pfromJust
            #$ (pfindOwnInput # (pfield @"inputs" # ctx.txInfo))
            # outref'
    spineIndex <- plet $ pfstBuiltin # (pasConstr # redm)
    perSpineChecks ctx.txInfo ownAddress datum redm spineIndex
  where
    f x y =
      PlutusTx.toBuiltinData $
        (func :: StageParams stage -> stage -> Interval POSIXTime)
          (unsafeFromBuiltinData x)
          (unsafeFromBuiltinData y)
      where
        -- func = stageToOnChainInterval @stage
        func _ _ = always -- TODO
    stageToOnChainInterval' :: Term s (PData :--> PData :--> PData)
    stageToOnChainInterval' = foreignImport $$(PlutusTx.compile [||f||])
    perSpineChecks txInfo ownAddress datum redm spineIndex =
      go [Prelude.minBound .. Prelude.maxBound]
      where
        go [] = pconstant ()
        go (spine : ss) = (checkSpineIf spine) (go ss)
        checkSpineIf spine cont =
          ( pif
              (spineIndex #== pconstant (Prelude.toInteger $ Prelude.fromEnum spine))
              (perTransitionCheck txInfo ownAddress datum redm spine)
              cont
          )
    perTransitionCheck txInfo ownAddress datum transition transitionSpine = P.do
      let validRange = pfromData $ pfield @"validRange" # txInfo
      stageParams <- plet $ datumTupleOf 0
      -- expectedInterval <- plet palways
      expectedInterval <- plet $ punsafeCoerce $ stageToOnChainInterval' # stageParams # (pconstant $ PlutusTx.toData stage)
      -- _ <- plet $ transitionSpec' # (datumTupleOf 1) # (datumTupleOf 2) # transition
      ptraceInfo (pconstant $ fromString $ "Checking transition " <> Prelude.show transitionSpine) $
        pif
          (pcontains # expectedInterval # validRange)
          constraintChecks
          (ptraceInfoError "Wrong interval for transition stage")
      where
        -- TODO: fold better
        constraintChecks =
          pif
            ( foldr (\x y -> pand' # x # y) (pconstant True) $
                map compileConstr constrs
            )
            (pconstant ())
            (ptraceInfoError "Constraint error")
        compileConstr c =
          ptraceInfoIfFalse (pconstant $ fromString $ "Checking constraint " <> Prelude.show c) $
            case c of
              TxFan fanKind fanSpec _value ->
                let
                  resolve =
                    pmap # plam (\x -> pfromData $ pfield @"resolved" # x)
                  fanList :: Term s (PBuiltinList PTxOut)
                  fanList = case fanKind of
                    In ->
                      resolve #$ pfromData $ pfield @"inputs" # txInfo
                    InRef -> resolve #$ pfield @"referenceInputs" # txInfo
                    Out -> pfromData $ pfield @"outputs" # txInfo
                  predicate = plam $ \txOut -> case fanSpec of
                    UserAddress pkhDsl ->
                      (ppkhAddress #$ punsafeCoerce $ compileDsl pkhDsl)
                        #== pfromData (pfield @"address" # txOut)
                    SameScript expectedState -> P.do
                      datum'' <-
                        pmatch $ pfromData (pfield @"datum" # txOut)
                      case datum'' of
                        POutputDatum datum' -> P.do
                          PDatum datum <-
                            pmatch $ pfromData $ pfield @"outputDatum" # datum'
                          let state = getRecordField 2 datum
                          pand'
                              # (ownAddress #== pfromData (pfield @"address" # txOut))
                              # (compileDsl expectedState #== state)
                        _ -> pconstant False
                 in
                  pnot # (pnull #$ pfilter # predicate # fanList)
              AdditionalSigner dsl ->
                let signatories = pfromData $ pfield @"signatories" # txInfo
                 in pelem # (punsafeCoerce $ pdata (compileDsl dsl)) # signatories
        params = datumTupleOf 1
        state = datumTupleOf 2
        compileDsl :: forall script1 x. ConstraintDSL script1 x -> Term s PData
        compileDsl dsl = case dsl of
          Pure x -> pconstant $ PlutusTx.toData x
          -- XXX: returns PBuiltinList PData in fact
          Ask @cvar @_ @dt Proxy ->
            case sing @cvar of
              SCParams -> params
              SCState -> state
              SCTransition -> transition
              _ -> Prelude.error "TODO"
          GetField @_ @y @_ @value dsl proxyLabel ->
            getRecordField
              (fieldNum @_ @y @x proxyLabel)
              (compileDsl dsl)
          IsOnChain -> punsafeCoerce $ pconstant True
          OfSpine spine _ ->
            ptraceInfo ("OfSpine") $
              punsafeCoerce $
                pconstrBuiltin
                  # pconstant (Prelude.toInteger $ Prelude.fromEnum spine)
                  # pconstant []
          x -> Prelude.error $ "TODO " <> Prelude.show x
        Just constrs = Map.lookup transitionSpine (transitionSpec' @script)
        datumTupleOf ix = getRecordField ix datum
        getRecordField ix d = ptryIndex ix $ psndBuiltin # (pasConstr # d)
        Just (stage, _, _) = Map.lookup transitionSpine (transitionStage proxy)

pscriptHashAddress :: Term s (PAsData PScriptHash :--> PAddress)
pscriptHashAddress = plam $ \datahash ->
  let credential = pcon (PScriptCredential (pdcons @"_0" # datahash #$ pdnil))
      nothing = pdata $ pcon (PDNothing pdnil)
      inner = pdcons @"credential" # pdata credential #$ pdcons @"stakingCredential" # nothing #$ pdnil
   in pcon (PAddress inner)

ppkhAddress :: Term s (PAsData PPubKeyHash :--> PAddress)
ppkhAddress = plam $ \datahash ->
  let credential = pcon (PPubKeyCredential (pdcons @"_0" # datahash #$ pdnil))
      nothing = pdata $ pcon (PDNothing pdnil)
      inner = pdcons @"credential" # pdata credential #$ pdcons @"stakingCredential" # nothing #$ pdnil
   in pcon (PAddress inner)

-- Various hacks and type annotations are done due to Plutus limitations
-- Data family - not supported -
-- https://github.com/IntersectMBO/plutus/issues/5768
-- Type family mentioning: https://github.com/IntersectMBO/plutus/issues/5769
