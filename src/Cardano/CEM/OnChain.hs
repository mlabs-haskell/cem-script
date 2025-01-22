{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
-- This warnings work incorrectly in presence of our Plutus code
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

module Cardano.CEM.OnChain (
  CEMScriptCompiled (..),
  genericPlutarchScript,
) where

import Cardano.CEM.DSL (
  CEMScript,
  CEMScriptSpec,
  ConstraintDSL (..),
  RecordLabel (MkRecordLabel),
  RecordSetter ((::=)),
  SCVar (SCComp, SCParams, SCState, SCTransition),
  SameScriptArg (MkSameScriptArg),
  TxConstraint (
    Error,
    If,
    MainSignerCoinSelect,
    MainSignerNoValue,
    MatchBySpine,
    Noop,
    Utxo
  ),
  Utxo (SameScript, UserAddress),
  UtxoKind (In, InRef, Out),
 )
import Data.Map qualified as Map
import Data.Singletons (Proxy (..), SingI (sing))
import Data.Spine (
  HasPlutusSpine,
  HasSpine (Spine),
  fieldNum,
  spineFieldsNum,
 )
import Data.String (IsString (..))
import Plutarch (
  ClosedTerm,
  Script,
  Term,
  pdelay,
  perror,
  pforce,
  phoistAcyclic,
  plam,
  plet,
  pmatch,
  (#),
  (#$),
  type (:-->),
 )
import Plutarch.Bool (
  PBool (..),
  PEq ((#==)),
  PPartialOrd ((#<=)),
  pand',
  pif,
  (#&&),
 )
import Plutarch.Builtin (
  PAsData,
  PBuiltinList,
  PData,
  PIsData (pdataImpl, pfromDataImpl),
  pasConstr,
  pconstrBuiltin,
  pdata,
  pfromData,
  pfstBuiltin,
  psndBuiltin,
 )
import Plutarch.Extras (
  getOwnAddress,
  pMkAdaOnlyValue,
  ppkhAddress,
 )
import Plutarch.FFI (foreignImport)
import Plutarch.LedgerApi (
  AmountGuarantees (NonZero),
  KeyGuarantees (Sorted, Unsorted),
  PDatum (PDatum),
  POutputDatum (POutputDatum),
  PScriptContext,
  PTxInfo,
  PTxOut,
  PValue,
 )
import Plutarch.LedgerApi.AssocMap qualified as PMap
import Plutarch.LedgerApi.Value (
  passertSorted,
  pforgetPositive,
  pforgetSorted,
 )
import Plutarch.List (
  PListLike (pcons, phead, pnil, pnull),
  pelem,
  pfilter,
  pfoldr,
  pmap,
  ptryIndex,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude (
  PInteger,
  PUnit,
  pconstant,
  pfield,
  pletFields,
  pshow,
  ptraceDebug,
  ptraceInfo,
  ptraceInfoError,
  ptraceInfoIfFalse,
 )
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (BuiltinData)
import PlutusTx qualified
import Text.Show.Pretty (ppShow)
import Prelude

-- | Interface of a compiled CEM Script.
class (CEMScript script) => CEMScriptCompiled script where
  -- | Code, original error message
  -- TODO: track transitions along with the errors
  errorCodes :: Proxy script -> [(String, String)]

  cemScriptCompiled :: Proxy script -> Script

-- | On-chain compilation logic.
genericPlutarchScript ::
  forall script.
  (CEMScript script) =>
  CEMScriptSpec False script ->
  (Maybe (PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData))) ->
  ClosedTerm (PData :--> PData :--> PAsData PScriptContext :--> PUnit)
genericPlutarchScript spec code =
  phoistAcyclic $ plam main
  where
    main ::
      forall s.
      Term s PData ->
      Term s PData ->
      Term s (PAsData PScriptContext) ->
      Term s PUnit
    main datum redm ctx' = P.do
      ctx <- pletFields @'["txInfo"] ctx'
      ownAddress <- plet $ getOwnAddress # ctx'
      spineIndex <- plet $ pfstBuiltin # (pasConstr # redm)
      -- We _always_ delay `comp` and force it _only_ when a user asks for
      -- its result and this is the moment when Nothing should error.
      -- TODO: Can we prevent user from calling `askC @CComp` if
      -- 'transitionComp' is not set?
      comp <- plet $ pdelay $ case code of
        Just x ->
          let
            script :: ClosedTerm (PData :--> PData :--> PData :--> PData)
            script = foreignImport x
           in
            script # params # state # redm
        Nothing -> ptraceInfo "transitionComp is nothing" perror
      -- Builds checks for transition (its spine)
      let checks = perTransitionCheck ctx.txInfo ownAddress comp
      -- Checks for transition spine from redeemer
      compileSpineCaseSwitch spineIndex checks
      where
        -- We are done

        -- Builds 'Spine (Transition script) -> Term s PUnit'
        perTransitionCheck txInfo ownAddress comp transitionSpine = P.do
          ptraceDebug
            (pconstant $ fromString $ "Checking transition " <> Prelude.show transitionSpine)
            perTransitionCheck'
          where
            perTransitionCheck' = P.do
              pif
                -- Constraints
                ( foldr
                    ((\x y -> pand' # x # y) . compileConstr)
                    (pconstant True)
                    constrs
                )
                -- Common checks
                (commonChecks # pfromData txInfo)
                -- Fail
                (ptraceInfoError "Constraint check failed")

            -- Get constraints from the definition
            constrs = case Map.lookup transitionSpine spec of
              Just x -> x
              Nothing ->
                error $
                  "Compilation error: transition: "
                    <> (Prelude.show transitionSpine)
                    <> " lacks spec"

            -- Actual constraint compilation
            compileConstr :: TxConstraint False script -> Term s PBool
            compileConstr c = ptraceInfoIfFalse
              (pconstant $ fromString $ "Checking constraint " <> Prelude.show c)
              $ case c of
                --
                MainSignerCoinSelect pkhDsl inValueDsl outValueDsl ->
                  P.do
                    let
                      txIns = resolve #$ pfromData $ pfield @"inputs" # txInfo
                      txOuts = pfromData $ pfield @"outputs" # txInfo
                    punsafeCoerce (compileDsl inValueDsl)
                      #<= (txFansValue txIns)
                      #&& (punsafeCoerce (compileDsl outValueDsl) #<= (txFansValue txOuts))
                  where
                    merge ::
                      Term
                        s
                        ( PValue Unsorted NonZero
                            :--> ( (PValue Sorted NonZero)
                                    :--> PValue Sorted NonZero
                                 )
                        )
                    merge = plam $ \x y -> ((passertSorted # x) <> y)
                    mapGetValues ::
                      Term
                        s
                        (PBuiltinList PTxOut :--> PBuiltinList (PValue Unsorted NonZero))
                    mapGetValues =
                      pmap
                        # plam (\x -> pforgetSorted $ pforgetPositive $ pfromData $ pfield @"value" # x)
                    resolve =
                      pmap # plam (\x -> pfromData $ pfield @"resolved" # x)
                    predicate :: Term s (PTxOut :--> PBool)
                    predicate = plam $ \txOut ->
                      (ppkhAddress #$ punsafeCoerce $ compileDsl pkhDsl)
                        #== pfromData (pfield @"address" # txOut)
                    txFansValue txIns =
                      let validTxIns = pfilter # predicate # txIns
                       in pfoldr
                            # merge
                            # (passertSorted #$ pMkAdaOnlyValue # 0)
                            #$ mapGetValues
                            # validTxIns
                --
                Utxo fanKind fanSpec value ->
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
                        let
                          correctAddress =
                            (ppkhAddress #$ punsafeCoerce $ compileDsl pkhDsl)
                              #== pfromData (pfield @"address" # txOut)
                         in
                          correctAddress
                      SameScript (MkSameScriptArg expectedState) ->
                        pmatch (pfromData (pfield @"datum" # txOut)) $ \case
                          POutputDatum datum' -> P.do
                            PDatum fanDatum <-
                              pmatch $ pfromData $ pfield @"outputDatum" # datum'
                            let
                              fanParams = getRecordField 0 fanDatum
                              fanState = getRecordField 1 fanDatum
                            ( (ownAddress #== pfield @"address" # txOut)
                                #&& ( (checkDsl expectedState fanState)
                                        #&& fanParams
                                        #== params
                                    )
                              )
                          _ -> pconstant False
                   in
                    checkDsl
                      value
                      (punsafeCoerce $ pfield @"value" #$ phead #$ pfilter # predicate # fanList)
                --
                MainSignerNoValue dsl ->
                  let
                    signatories = pfromData $ pfield @"signatories" # txInfo
                    xSigner = punsafeCoerce $ pdata (compileDsl dsl)
                   in
                    ptraceInfoIfFalse (pshow xSigner) $
                      ptraceInfoIfFalse (pshow signatories) $
                        pelem # xSigner # signatories
                --
                Noop -> pconstant True
                --
                Error message -> ptraceInfoError $ pconstant message
                --
                If condDsl thenDsl elseDsl ->
                  pif
                    (pfromData $ punsafeCoerce $ compileDsl condDsl)
                    (compileConstr thenDsl)
                    (compileConstr elseDsl)
                --
                MatchBySpine valueDsl caseSwitch ->
                  let
                    value = punsafeCoerce $ compileDsl valueDsl
                    valueSpineNum = pfstBuiltin # (pasConstr # value)
                   in
                    compileSpineCaseSwitch
                      valueSpineNum
                      (compileConstr . (caseSwitch Map.!))

            -- Compiles and check the value
            checkDsl ::
              ConstraintDSL script1 x ->
              Term s PData ->
              Term s PBool
            checkDsl expectationDsl value =
              case expectationDsl of
                -- Trivial case
                Anything -> pconstant True
                -- Special case - compares spine and all fields that defined by setters
                UnsafeOfSpine spine setters ->
                  (pfstBuiltin #$ pasConstr # xValue)
                    #== pconstant (Prelude.toInteger $ Prelude.fromEnum spine)
                    #&& let
                          fields = (psndBuiltin #$ pasConstr # xValue)
                          ixAndSetters = zip [(0 :: Integer) ..] setters
                          perIxCheck (ix, (_ ::= fieldValueDsl)) =
                            checkDsl fieldValueDsl $ ptryIndex (fromInteger ix) fields
                          foldAnd (!x : xs) = x #&& (foldAnd xs)
                          foldAnd [] = pconstant True
                         in
                          foldAnd $ map perIxCheck ixAndSetters
                -- Standard case - evaluate and compare
                _ -> xValue #== value
              where
                xValue = compileDsl expectationDsl

            -- Compiles a DSL term into a value
            compileDsl :: forall script1 x. ConstraintDSL script1 x -> Term s PData
            compileDsl dsl = punsafeCoerce $ case dsl of
              Pure x -> pconstant $ PlutusTx.toData x
              IsOnChain -> compileDsl $ Pure True -- short-cut
              Ask @cvar @_ @dt Proxy ->
                case sing @cvar of
                  SCParams -> params
                  SCState -> state
                  SCTransition -> redm
                  SCComp -> pforce comp
              GetField @_ @y @_ @value valueDsl proxyLabel ->
                getRecordField
                  (fieldNum @y proxyLabel)
                  (compileDsl valueDsl)
              UnsafeOfSpine spine setters ->
                punsafeCoerce
                  $ pconstrBuiltin
                    # pconstant (Prelude.toInteger $ Prelude.fromEnum spine)
                    #$ foldr pcons' pnil
                  $ map fieldValue setters
                where
                  pcons' x y = pcons # x # y
                  fieldValue (_ ::= valueDsl) = compileDsl valueDsl
              -- TODO: Should we lift AsData functions?
              LiftPlutarch @_ @py plutrachFunc valueDsl ->
                let
                  x = pfromDataImpl $ punsafeCoerce $ compileDsl valueDsl
                 in
                  pdataImpl @py $ punsafeCoerce $ plutrachFunc # x
              LiftPlutarch2 @_ @_ @py plutarchFunc vDsl1 vDsl2 ->
                let
                  x = pfromDataImpl $ punsafeCoerce $ compileDsl vDsl1
                  y = pfromDataImpl $ punsafeCoerce $ compileDsl vDsl2
                 in
                  pdataImpl @py $
                    punsafeCoerce $
                      plutarchFunc x y
              Eq xDsl yDsl -> case (xDsl, yDsl) of
                (Anything, _) -> compileDsl $ Pure True
                (_, Anything) -> compileDsl $ Pure True
                (_, _) -> pdataImpl $ (compileDsl xDsl) #== (compileDsl yDsl)
              UnsafeUpdateOfSpine @_ @datatype notUpdatedValueDsl spine setters ->
                pmatch
                  ( (pfstBuiltin #$ pasConstr # notUpdatedValue)
                      #== pconstant (Prelude.toInteger $ Prelude.fromEnum spine)
                  )
                  $ \case
                    PTrue ->
                      if length setters == 0
                        then notUpdatedValue
                        else
                          punsafeCoerce
                            $ pconstrBuiltin
                              # pconstant (Prelude.toInteger $ Prelude.fromEnum spine)
                              #$ foldr pcons' pnil
                            $ map perIxValue [0 .. (toInteger (spineFieldsNum spine) - 1)]
                    PFalse -> ptraceInfoError "Spines do not match"
                where
                  pcons' x y = pcons # x # y
                  notUpdatedValue = compileDsl notUpdatedValueDsl
                  notUpdatedFields = (psndBuiltin #$ pasConstr # notUpdatedValue)
                  updatedFields =
                    Map.fromList
                      [ ( toInteger $ fieldNum @datatype (Proxy @label)
                        , compileDsl valueDsl
                        )
                      | (MkRecordLabel @label) ::= valueDsl <- setters
                      ]
                  perIxValue ix = case updatedFields Map.!? ix of
                    Just value -> value
                    Nothing -> ptryIndex (fromInteger ix) notUpdatedFields
              Anything ->
                error $
                  "Non-deterministic code in place it should not be "
                    <> " while compiling on-chain: \n"
                    <> ppShow dsl

        -- Datum deconstruction
        params = datumTupleOf 0
        state = datumTupleOf 1
        datumTupleOf ix = getRecordField ix datum
        getRecordField ix d = ptryIndex ix $ psndBuiltin # (pasConstr # d)

{- | Tries all spines from 'Spine sop' and if it matches the index
calculates 'Term s x' by applying given `caseSwitchFunc' to the spine.
-}
compileSpineCaseSwitch ::
  forall x sop s.
  (HasPlutusSpine sop) =>
  Term s PInteger ->
  (Spine sop -> Term s x) ->
  Term s x
compileSpineCaseSwitch spineIndex caseSwitchFunc =
  go [Prelude.minBound .. Prelude.maxBound]
  where
    go [] = perror
    go (spine : ss) = (checkSpineIf spine) (go ss)
    checkSpineIf !spine !cont =
      ( pif
          (spineIndex #== pconstant (Prelude.toInteger $ Prelude.fromEnum spine))
          ( ptraceDebug
              ( pconstant $
                  fromString $
                    "Matched spine: " <> Prelude.show spine
              )
              (caseSwitchFunc spine)
          )
          cont
      )

-- | Currently checks that staking features are not used.
commonChecks :: Term s (PTxInfo :--> PUnit)
commonChecks = plam go
  where
    go :: Term s1 PTxInfo -> Term s1 PUnit
    go txInfo =
      pif
        (stakingStuffDetected txInfo)
        (pconstant ())
        (ptraceInfo "Staking feature was used: currently not supported" perror)
    stakingStuffDetected :: Term s1 PTxInfo -> Term s1 PBool
    stakingStuffDetected txInfo =
      (pnull # pfromData (pfield @"dcert" # txInfo))
        #&& (PMap.pnull #$ pfromData (pfield @"wdrl" # txInfo))
