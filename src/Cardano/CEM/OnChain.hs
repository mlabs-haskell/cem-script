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

import Cardano.CEM hiding (compileDsl)
import Data.Map qualified as Map
import Data.Singletons
import Data.Spine
import Data.String (IsString (..))
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Extras
import Plutarch.FFI (foreignImport)
import Plutarch.LedgerApi
import Plutarch.LedgerApi.AssocMap qualified as PMap
import Plutarch.LedgerApi.Value
import Plutarch.List
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (BuiltinData)
import PlutusTx qualified
import Text.Show.Pretty (ppShow)
import Prelude

-- Interfaces

class (CEMScript script) => CEMScriptCompiled script where
  -- | Code, original error message
  -- FIXME: track transition it might be raised
  errorCodes :: Proxy script -> [(String, String)]

  cemScriptCompiled :: Proxy script -> Script

-- Compilation
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
      comp <- plet $ pdelay $ case code of
        Just x ->
          let
            script :: ClosedTerm (PData :--> PData :--> PData :--> PData)
            script = foreignImport x
           in
            script # params # state # redm
        Nothing -> ptraceInfo "Unreachable" perror
      perSpineChecks ctx.txInfo ownAddress comp spineIndex
      where
        params = datumTupleOf 0
        state = datumTupleOf 1
        datumTupleOf ix = getRecordField ix datum
        getRecordField ix d = ptryIndex ix $ psndBuiltin # (pasConstr # d)
        perSpineChecks txInfo ownAddress comp spineIndex =
          compileSpineCaseSwitch spineIndex f
          where
            f = perTransitionCheck txInfo ownAddress redm comp
        perTransitionCheck txInfo ownAddress transition comp transitionSpine = P.do
          ptraceDebug
            (pconstant $ fromString $ "Checking transition " <> Prelude.show transitionSpine)
            constraintChecks
          where
            -- FIXME: fold better
            constraintChecks = P.do
              pif
                (foldr ((\x y -> pand' # x # y) . compileConstr) (pconstant True) constrs)
                (commonChecks # pfromData txInfo)
                (ptraceInfoError "Constraint check failed")
            compileConstr :: TxConstraint False script -> Term s PBool
            compileConstr c =
              ptraceInfoIfFalse
                ( pconstant $ fromString $ "Checking constraint " <> Prelude.show c
                )
                $ case c of
                  MainSignerCoinSelect pkhDsl inValueDsl outValueDsl ->
                    P.do
                      -- FIXME: check final difference
                      -- FIXME: DRY with TxSpec implemenation
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
                  TxFan fanKind fanSpec value ->
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
                        SameScript expectedState ->
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
                      -- FIXME: do not use phead
                      checkDsl
                        value
                        (punsafeCoerce $ pfield @"value" #$ phead #$ pfilter # predicate # fanList)
                  MainSignerNoValue dsl ->
                    let
                      signatories = pfromData $ pfield @"signatories" # txInfo
                      xSigner = punsafeCoerce $ pdata (compileDsl dsl)
                     in
                      ptraceInfoIfFalse (pshow xSigner) $
                        ptraceInfoIfFalse (pshow signatories) $
                          pelem # xSigner # signatories
                  Noop -> pconstant True
                  Error message -> ptraceInfoError $ pconstant message
                  If condDsl thenDsl elseDsl ->
                    pif
                      (pfromData $ punsafeCoerce $ compileDsl condDsl)
                      (compileConstr thenDsl)
                      (compileConstr elseDsl)
                  MatchBySpine valueDsl caseSwitch ->
                    let
                      value = punsafeCoerce $ compileDsl valueDsl
                      valueSpineNum = pfstBuiltin # (pasConstr # value)
                     in
                      compileSpineCaseSwitch
                        valueSpineNum
                        (compileConstr . (caseSwitch Map.!))
            checkDsl ::
              ConstraintDSL script1 x ->
              Term s PData ->
              Term s PBool
            checkDsl expectationDsl value =
              case expectationDsl of
                Anything -> pconstant True
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
                          foldAnd $
                            map perIxCheck ixAndSetters
                _ -> xValue #== value
              where
                xValue = compileDsl expectationDsl
            -- FIXME: Some typing? `newtype MyPData x`?
            -- ConstraintDSL script1 (PLifted x) -> Term s (AsData x)
            compileDsl :: forall script1 x. ConstraintDSL script1 x -> Term s PData
            compileDsl dsl = punsafeCoerce $ case dsl of
              Pure x -> pconstant $ PlutusTx.toData x
              IsOnChain -> compileDsl $ Pure True
              -- XXX: returns PBuiltinList PData in fact
              Ask @cvar @_ @dt Proxy ->
                case sing @cvar of
                  SCParams -> params
                  SCState -> state
                  SCTransition -> transition
                  -- FIXME: is this force good?
                  SCComp -> pforce comp
                  SCTxInfo -> pforgetData txInfo
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
              -- FIXME: Should we lift AsData functins?
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
                (_, _) ->
                  pdataImpl $ (compileDsl xDsl) #== (compileDsl yDsl)
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
                    -- FIXME: use error code
                    PFalse -> ptraceInfoError "Spines not matching"
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
              Anything -> nonDetMessage dsl
            nonDetMessage dsl =
              error $
                "Non-deterministic code in place it should not be "
                  <> " while compiling on-chain: \n"
                  <> ppShow dsl
            constrs = case Map.lookup transitionSpine spec of
              Just x -> x
              Nothing -> error "Compilation error: some spine lacks spec"

commonChecks :: Term s (PTxInfo :--> PUnit)
commonChecks = plam go
  where
    go :: Term s1 PTxInfo -> Term s1 PUnit
    go txInfo =
      pif
        (stackingStuffDisabled txInfo)
        (pconstant ())
        (ptraceInfo "Stacking feature used" perror)
    stackingStuffDisabled :: Term s1 PTxInfo -> Term s1 PBool
    stackingStuffDisabled txInfo =
      (pnull # pfromData (pfield @"dcert" # txInfo))
        #&& (PMap.pnull #$ pfromData (pfield @"wdrl" # txInfo))

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
