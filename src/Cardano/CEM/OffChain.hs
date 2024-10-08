{- | User-facing utilities for querying and sending Txs
on top of interfaces in `Monads` module
-}
module Cardano.CEM.OffChain where

import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.List (find, nub)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Spine (HasSpine (getSpine))

import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V2 (PubKeyHash, always, fromData)

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns, txOuts)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
  toPlutusData,
 )

import Plutarch.Script (serialiseScript)

-- Project imports

import Cardano.CEM
import Cardano.CEM.DSL
import Cardano.CEM.Monads
import Cardano.CEM.OnChain (CEMScriptCompiled (..), cemScriptAddress)
import Cardano.Extras

fromPlutusAddressInMonad ::
  (MonadBlockchainParams m) => Address -> m (AddressInEra Era)
fromPlutusAddressInMonad address = do
  networkId <- askNetworkId
  return $ fromPlutusAddress networkId address

checkTxIdExists :: (MonadQueryUtxo m) => TxId -> m Bool
checkTxIdExists txId = do
  result <- queryUtxo $ ByTxIns [TxIn txId (TxIx 0)]
  return $ not $ Map.null $ unUTxO result

awaitTx :: forall m. (MonadIO m, MonadQueryUtxo m) => TxId -> m ()
awaitTx txId = do
  go 5
  where
    go :: Integer -> m ()
    go 0 = liftIO $ fail "Tx was not awaited." -- FIXME
    go n = do
      exists <- checkTxIdExists txId
      liftIO $ threadDelay 1_000_000
      if exists
        then logEvent $ AwaitedTx txId
        else go $ n - 1

failLeft :: (MonadFail m, Show s) => Either s a -> m a
failLeft (Left errorMsg) = fail $ show errorMsg
failLeft (Right value) = return value

cemTxOutDatum ::
  (CEMScript script) => TxOut ctx Era -> Maybe (CEMScriptDatum script)
cemTxOutDatum txOut =
  fromData =<< toPlutusData <$> getScriptData <$> mTxOutDatum txOut

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
data Resolution
  = TxInR (TxIn, TxInWitness)
  | TxInRefR (TxIn, TxInWitness)
  | TxOutR (TxOut CtxTx Era)
  | AdditionalSignerR PubKeyHash
  | NoopR
  deriving stock (Show, Eq)

construct :: [Resolution] -> ResolvedTx
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
          perTransitionScriptSpec @script
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
  ExceptT TxResolutionError m Resolution
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
            SameScript state ->
              cemTxOutDatum txOut
                == Just
                  ( params
                  , state
                  )
            UserAddress {} -> True

      (address, outTxDatum) = case fanFilter of
        UserAddress pkh -> (pubKeyHashAddress pkh, TxOutDatumNone)
        SameScript state ->
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

resolveTx ::
  forall m.
  (MonadQueryUtxo m, MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (Either TxResolutionError ResolvedTx)
resolveTx spec = runExceptT $ do
  !resolutions <- mapM resolveSomeAction (actions spec)
  let resolvedTx = construct $ nub $ concat resolutions
  return $ resolvedTx {signer = specSigner spec}
  where
    resolveSomeAction ::
      SomeCEMAction -> (ExceptT TxResolutionError m) [Resolution]
    resolveSomeAction (MkSomeCEMAction @script action) = do
      let MkCEMAction params _ = action
      mScript <- lift $ queryScriptState params
      cs <- ExceptT $ return $ compileActionConstraints mScript action
      mapM (process action) cs

resolveTxAndSubmit ::
  (MonadQueryUtxo m, MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (Either TxResolutionError TxId)
resolveTxAndSubmit spec = do
  result <- runExceptT $ do
    resolved <- ExceptT $ resolveTx spec
    let result = submitResolvedTx resolved
    ExceptT $ first UnhandledSubmittingError <$> result
  logEvent $ SubmittedTxSpec spec result
  return result
