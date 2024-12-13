{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

{- | User-facing utilities for querying and sending Txs
on top of interfaces in `Monads` module
-}
module Cardano.CEM.OffChain where

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns, txOuts)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
  toPlutusData,
 )
import Cardano.CEM.Address (cemScriptAddress)
import Cardano.CEM.Compile (transitionInStateSpine)
import Cardano.CEM.DSL
import Cardano.CEM.Monads (
  BlockchainMonadEvent (AwaitedTx, SubmittedTxSpec),
  CEMAction (..),
  MonadBlockchainParams (askNetworkId, logEvent),
  MonadQueryUtxo (..),
  MonadSubmitTx (submitResolvedTxRet),
  ResolvedTx (..),
  SomeCEMAction (..),
  TransitionError (
    CannotFindTransitionInput,
    CompilationError,
    SpecExecutionError
  ),
  TxSpec (actions, specSigner),
  TxSubmittingError,
  UtxoQuery (ByAddresses, ByTxIns),
 )
import Cardano.CEM.OnChain (CEMScriptCompiled (..))
import Cardano.Extras (
  Era,
  PlutusLang,
  TxInWitness,
  fromPlutusAddress,
  fromPlutusValue,
  mTxOutDatum,
  mkInlineDatum,
  mkInlinedDatumScriptWitness,
  txOutValue,
  withKeyWitness,
 )
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Either.Extra (mapLeft, mapRight)
import Data.List (find, nub)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Singletons (sing)
import Data.Spine (HasSpine (..))
import Plutarch (Config (..), (#))
import Plutarch.Evaluate (evalTerm)
import Plutarch.Lift (pconstant, plift)
import Plutarch.Prelude (getField)
import Plutarch.Script (serialiseScript)
import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V2 (PubKeyHash, always, fromData)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Unsafe.Coerce (unsafeCoerce)
import Prelude

fromPlutusAddressInMonad ::
  (MonadBlockchainParams m) => Address -> m (AddressInEra Era)
fromPlutusAddressInMonad address = do
  networkId <- askNetworkId
  return $ fromPlutusAddress networkId address

failLeft :: (MonadFail m, Show s) => Either s a -> m a
failLeft (Left errorMsg) = fail $ show errorMsg
failLeft (Right value) = return value

cemTxOutDatum ::
  (CEMScript script) => TxOut ctx Era -> Maybe (CEMScriptDatum script)
cemTxOutDatum txOut = fromData . (toPlutusData <$> getScriptData) =<< mTxOutDatum txOut

cemTxOutState :: (CEMScriptCompiled script) => TxOut ctx Era -> Maybe (State script)
cemTxOutState txOut = snd <$> cemTxOutDatum txOut

queryScriptTxInOut ::
  forall m script.
  ( MonadQueryUtxo m
  , CEMScriptCompiled script
  ) =>
  Params script ->
  m (Maybe (TxIn, TxOut CtxUTxO Era))
queryScriptTxInOut params = do
  utxo <- queryUtxo $ ByAddresses [scriptAddress]
  let mScriptTxIn =
        case Map.assocs $ unUTxO utxo of
          [] -> Nothing
          pairs -> find hasSameParams pairs
      hasSameParams (_, txOut) =
        case cemTxOutDatum txOut of
          Just (p, _) -> params == p
          Nothing -> False -- May happen in case of changed Datum encoding
  return mScriptTxIn
  where
    scriptAddress = cemScriptAddress (Proxy :: Proxy script)

queryScriptState ::
  forall m script.
  ( MonadQueryUtxo m
  , CEMScriptCompiled script
  ) =>
  Params script ->
  m (Maybe (State script))
queryScriptState params = do
  mTxInOut <- queryScriptTxInOut params
  return (cemTxOutState . snd =<< mTxInOut)

-- FIXME: doc, naming
data OffchainTxIR
  = TxInR (TxIn, TxInWitness)
  | TxInRefR (TxIn, TxInWitness)
  | TxOutR (TxOut CtxTx Era)
  | AdditionalSignerR PubKeyHash
  | NoopR
  deriving stock (Show, Eq)

construct :: [OffchainTxIR] -> ResolvedTx
construct rs = constructGo rs emptyResolvedTx
  where
    emptyResolvedTx =
      MkResolvedTx
        { txIns = []
        , txInRefs = []
        , txOuts = []
        , toMint = TxMintNone
        , additionalSigners = []
        , signer = error "TODO: Unreachable laziness trick"
        , -- FIXME
          interval = always
        }
    constructGo (r : rest) !acc =
      let newAcc = case r of
            TxInR x -> acc {txIns = x : txIns acc}
            TxInRefR x -> acc {txInRefs = fst x : txInRefs acc}
            TxOutR x -> acc {txOuts = x : txOuts acc}
            AdditionalSignerR s ->
              acc {additionalSigners = s : additionalSigners acc}
            NoopR -> acc
       in constructGo rest newAcc
    constructGo [] !acc = acc

compileActionConstraints ::
  forall script.
  (CEMScriptCompiled script) =>
  Maybe (State script) ->
  CEMAction script ->
  Either TxResolutionError [TxConstraint True script]
compileActionConstraints
  mState
  (MkCEMAction params transition) =
    runExcept $ do
      let
        uncompiled =
          transitionSpec @script
            Map.! getSpine transition
        xSpine = transitionInStateSpine uncompiled

      when (fmap getSpine mState /= xSpine) $
        throwError CEMScriptTxInResolutionError

      let
        -- FIXME: fromJust laziness trick
        datum = (params, fromJust mState)
        compiled' = map (compileConstraint datum transition) uncompiled

      -- FIXME: raise lefts from compiled
      let f x = case x of
            Left message -> throwError $ PerTransitionErrors [CompilationError message]
            Right x' -> return x'
      -- FIXME: add resolution logging
      mapM f compiled'

process ::
  forall script m.
  (MonadQueryUtxo m, CEMScriptCompiled script) =>
  CEMAction script ->
  TxConstraint True script ->
  ExceptT TxResolutionError m OffchainTxIR
process (MkCEMAction params transition) ec = case ec of
  Noop -> return NoopR
  c@MainSignerCoinSelect {} -> do
    utxo <- lift $ queryUtxo $ ByAddresses [pubKeyHashAddress $ user c]
    let utxoPairs =
          map (withKeyWitness . fst) $ Map.toList $ unUTxO utxo
    -- FIXME: do actuall coin selection
    return $ TxInR $ head utxoPairs
  (TxFan kind fanFilter value) -> do
    case kind of
      Out -> do
        let value' = convertTxOut $ fromPlutusValue value
        address' <- lift $ fromPlutusAddressInMonad address
        return $
          TxOutR $
            TxOut address' value' outTxDatum ReferenceScriptNone
      someIn -> do
        utxo <- lift $ queryUtxo $ ByAddresses [address]
        let
          utxoPairs = Map.toList $ unUTxO utxo
          matchingUtxos =
            map (addWittness . fst) $ filter predicate utxoPairs
        case matchingUtxos of
          x : _ -> return $ case someIn of
            -- FIXME: log/fail on >1 options to choose for script
            In -> TxInR x
            InRef -> TxInRefR x
          [] ->
            throwError $ PerTransitionErrors [CannotFindTransitionInput]
    where
      predicate (_, txOut) =
        txOutValue txOut == fromPlutusValue value
          && case fanFilter of
            -- FIXME: refactor DRY
            SameScript (MkSameScriptArg state) ->
              cemTxOutDatum txOut
                == Just
                  ( params
                  , state
                  )
            UserAddress {} -> True

      (address, outTxDatum) = case fanFilter of
        UserAddress pkh -> (pubKeyHashAddress pkh, TxOutDatumNone)
        SameScript (MkSameScriptArg state) ->
          ( scriptAddress
          , mkInlineDatum
              ( params
              , state
              )
          )
      -- FIXME: understand what is happening
      convertTxOut x =
        TxOutValueShelleyBased shelleyBasedEra $ toMaryValue x
      addWittness = case fanFilter of
        UserAddress {} -> withKeyWitness
        SameScript {} -> (,scriptWitness)
        where
          scriptWitness =
            mkInlinedDatumScriptWitness
              (PlutusScriptSerialised @PlutusLang $ serialiseScript script)
              transition
  MainSignerNoValue pkh -> return $ AdditionalSignerR pkh
  Error message ->
    throwError $
      PerTransitionErrors [SpecExecutionError $ show message]
  where
    script = cemScriptCompiled (Proxy :: Proxy script)
    scriptAddress = cemScriptAddress (Proxy :: Proxy script)

-- -----------------------------------------------------------------------------
-- Transaction resolving
-- -----------------------------------------------------------------------------

data TxResolutionError
  = CEMScriptTxInResolutionError
  | -- FIXME: record transition and action involved
    PerTransitionErrors [TransitionError]
  | -- FIXME: this is weird
    UnhandledSubmittingError TxSubmittingError
  deriving stock (Show)

resolveTx ::
  forall m.
  (MonadQueryUtxo m) =>
  TxSpec ->
  m (Either TxResolutionError ResolvedTx)
resolveTx spec = runExceptT $ do
  !resolutions <- mapM resolveSomeAction (actions spec)
  let resolvedTx = construct $ nub $ concat resolutions
  return $ resolvedTx {signer = specSigner spec}
  where
    resolveSomeAction ::
      SomeCEMAction -> (ExceptT TxResolutionError m) [OffchainTxIR]
    resolveSomeAction (MkSomeCEMAction @script action) = do
      let MkCEMAction params _ = action
      mScript <- lift $ queryScriptState params
      cs <- ExceptT $ return $ compileActionConstraints mScript action
      mapM (process action) cs

resolveTxAndSubmitRet ::
  (MonadQueryUtxo m, MonadSubmitTx m) =>
  TxSpec ->
  m (Either TxResolutionError (TxBodyContent BuildTx Era, TxBody Era, TxInMode, UTxO Era))
resolveTxAndSubmitRet spec = do
  result <- runExceptT $ do
    resolved <- ExceptT $ resolveTx spec
    let result = submitResolvedTxRet resolved
    ExceptT $ first UnhandledSubmittingError <$> result
  logEvent $ SubmittedTxSpec spec (mapLeft (const ()) $ mapRight (getTxId . (\(_, a, _, _) -> a)) result)
  return result

resolveTxAndSubmit ::
  (MonadQueryUtxo m, MonadSubmitTx m) =>
  TxSpec ->
  m (Either TxResolutionError TxId)
resolveTxAndSubmit spec = do
  mapRight (getTxId . (\(_, a, _, _) -> a)) <$> resolveTxAndSubmitRet spec

-- move away

awaitTx :: forall m. (MonadIO m, MonadQueryUtxo m) => TxId -> m ()
awaitTx txId = do
  go 5
  where
    go :: Integer -> m ()
    go 0 = liftIO $ fail "Tx was not awaited." -- FIXME:
    go n = do
      exists <- checkTxIdExists
      liftIO $ threadDelay 1_000_000
      if exists
        then logEvent $ AwaitedTx txId
        else go $ n - 1

    checkTxIdExists :: (MonadQueryUtxo m) => m Bool
    checkTxIdExists = do
      result <- queryUtxo $ ByTxIns [TxIn txId (TxIx 0)]
      return $ not $ Map.null $ unUTxO result

---

-- TODO: add note on datums and transitions
compileConstraint ::
  forall script.
  (CEMScript script) =>
  CEMScriptDatum script ->
  Transition script ->
  TxConstraint False script ->
  Either String (TxConstraint True script)
compileConstraint datum transition c = case c of
  If condDsl thenConstr elseConstr -> do
    value <- compileDslRecur condDsl
    if value
      then recur thenConstr
      else recur elseConstr
  MatchBySpine value caseSwitch ->
    recur . (caseSwitch Map.!) . getSpine =<< compileDslRecur value
  MainSignerNoValue signerDsl ->
    MainSignerNoValue <$> compileDslRecur signerDsl
  MainSignerCoinSelect pkhDsl inValueDsl outValueDsl ->
    MainSignerCoinSelect
      <$> compileDslRecur pkhDsl
      <*> compileDslRecur inValueDsl
      <*> compileDslRecur outValueDsl
  TxFan kind fanFilter valueDsl ->
    TxFan kind <$> compileFanFilter fanFilter <*> compileDslRecur valueDsl
  Noop -> Right Noop
  -- XXX: changing resolved type param of Error
  e@(Error {}) -> Right $ unsafeCoerce e
  where
    compileDslRecur :: ConstraintDSL script x -> Either String x
    compileDslRecur = compileDsl @script datum transition
    recur = compileConstraint @script datum transition
    compileFanFilter :: TxFanFilter 'False script -> Either String (TxFanFilter 'True script)
    compileFanFilter fanFilter = case fanFilter of
      UserAddress dsl -> UserAddress <$> compileDslRecur dsl
      SameScript (MkSameScriptArg stateDsl) -> SameScript . MkSameScriptArg <$> compileDslRecur stateDsl

-- TODO: types errors
compileDsl ::
  forall script x.
  (CEMScript script) =>
  CEMScriptDatum script ->
  Transition script ->
  ConstraintDSL script x ->
  Either String x
compileDsl datum@(params, state) transition dsl = case dsl of
  Pure x -> Right x
  Ask @cvar @_ @dt Proxy ->
    case sing @cvar of
      SCParams -> Right params
      SCState -> Right state
      SCTransition -> Right transition
      SCComp -> case transitionComp @script of
        Just go -> Right $ go params state transition
        Nothing -> error "Unreachable"
      SCTxInfo -> raiseOnchainErrorMessage ("TxInfo reference" :: String)
  IsOnChain -> Right False
  GetField @label @datatype @_ @value recordDsl _ -> do
    recordValue <- recur recordDsl
    Right $ getField @label @datatype @value recordValue
  Eq @v xDsl yDsl -> (==) <$> (recur @v) xDsl <*> (recur @v) yDsl
  UnsafeOfSpine spine recs -> do
    rs <- mapM compileRecordSetter recs
    Right $
      fromJust . PlutusTx.fromData . PlutusTx.builtinDataToData $
        PlutusTx.mkConstr
          (toInteger $ fromEnum spine)
          rs
    where
      compileRecordSetter (_ ::= valueDsl) = do
        value <- recur valueDsl
        Right $ PlutusTx.toBuiltinData value
  UnsafeUpdateOfSpine valueDsl _spine setters -> do
    case setters of
      [] -> recur valueDsl
      _ -> error "FIXME: not implemented"
  LiftPlutarch pterm argDsl -> do
    arg <- recur argDsl
    case evalTerm NoTracing $ pterm # pconstant arg of
      Right (Right resultTerm, _, _) -> Right $ plift resultTerm
      Right (Left message, _, _) ->
        Left $ "Unreachable: plutach running error " <> show message
      Left message -> Left $ "Unreachable: plutach running error " <> show message
  LiftPlutarch2 pterm arg1Dsl arg2Dsl -> do
    arg1 <- recur arg1Dsl
    arg2 <- recur arg2Dsl
    case evalTerm NoTracing $ pterm (pconstant arg1) (pconstant arg2) of
      Right (Right resultTerm, _, _) -> Right $ plift resultTerm
      Right (Left message, _, _) ->
        Left $ "Unreachable: plutach running error " <> show message
      Left message -> Left $ "Unreachable: plutach running error " <> show message
  Anything -> raiseOnchainErrorMessage dsl
  where
    raiseOnchainErrorMessage :: (Show a) => a -> Either String x
    raiseOnchainErrorMessage x =
      Left $
        "On-chain only feature was reached while off-chain constraints compilation "
          <> "(should be guarded to only triggered onchain): "
          <> show x
    recur :: ConstraintDSL script x1 -> Either String x1
    recur = compileDsl @script datum transition
