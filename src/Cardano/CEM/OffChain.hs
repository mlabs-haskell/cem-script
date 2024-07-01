{- | User-facing utilities for querying and sending Txs
on top of interfaces in `Monads` module
-}
module Cardano.CEM.OffChain where

import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.List (find)
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V2 (
  UnsafeFromData (..),
  always,
  fromData,
 )

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
  toPlutusData,
 )

-- Project imports

import Cardano.CEM
import Cardano.CEM.Monads
import Cardano.CEM.OnChain (CEMScriptCompiled (..), cemScriptAddress)
import Cardano.Extras
import Data.Spine (HasSpine (getSpine))

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

-- TODO: use regular CEMScript
cemTxOutDatum :: (CEMScriptCompiled script) => TxOut ctx Era -> Maybe (CEMScriptDatum script)
cemTxOutDatum txOut =
  fromData =<< toPlutusData <$> getScriptData <$> mTxOutDatum txOut

cemTxOutState :: (CEMScriptCompiled script) => TxOut ctx Era -> Maybe (State script)
cemTxOutState txOut =
  let
    getState (_, _, state) = state
   in
    getState <$> cemTxOutDatum txOut

queryScriptTxInOut ::
  forall m script.
  ( MonadQueryUtxo m
  , CEMScriptCompiled script
  ) =>
  CEMParams script ->
  m (Maybe (TxIn, TxOut CtxUTxO Era))
queryScriptTxInOut params = do
  utxo <- queryUtxo $ ByAddresses [scriptAddress]
  let mScriptTxIn =
        case Map.assocs $ unUTxO utxo of
          [] -> Nothing
          pairs -> find hasSameParams pairs
      hasSameParams (_, txOut) =
        case cemTxOutDatum txOut of
          Just (p1, p2, _) -> params == MkCEMParams p2 p1
          Nothing -> False -- May happen in case of changed Datum encoding
  return mScriptTxIn
  where
    scriptAddress = cemScriptAddress (Proxy :: Proxy script)

queryScriptState ::
  forall m script.
  ( MonadQueryUtxo m
  , CEMScriptCompiled script
  ) =>
  CEMParams script ->
  m (Maybe (State script))
queryScriptState params = do
  mTxInOut <- queryScriptTxInOut params
  return (cemTxOutState . snd =<< mTxInOut)

resolveAction ::
  forall m.
  (MonadQueryUtxo m, MonadSubmitTx m) =>
  SomeCEMAction ->
  m (Either TxResolutionError ResolvedTx)
resolveAction
  someAction@(MkSomeCEMAction @script (MkCEMAction params transition)) =
    -- Add script TxIn

    runExceptT $ do
      mScriptTxIn' <- lift $ queryScriptTxInOut params

      let
        -- TODO
        mScriptTxIn = case transitionStage (Proxy :: Proxy script) Map.! getSpine transition of
          (_, Nothing, _) -> Nothing
          _ -> mScriptTxIn'
        mState = cemTxOutState =<< snd <$> mScriptTxIn
        witnesedScriptTxIns =
          case mScriptTxIn of
            Just (txIn, _) ->
              let
                scriptWitness =
                  mkInlinedDatumScriptWitness
                    (PlutusScriptSerialised @PlutusLang script)
                    transition
               in
                [(txIn, scriptWitness)]
            Nothing -> []

      scriptTransition <- case transitionSpec (scriptParams params) mState transition of
        Left errorMessage ->
          throwError $
            MkTransitionError someAction (StateMachineError $ show errorMessage)
        Right result -> return result

      -- Coin-select

      let
        byKind kind =
          filter (\x -> txFanCKind x == kind) $
            constraints scriptTransition

      txInsPairs <- concat <$> mapM resolveTxIn (byKind In)
      txOuts <- concat <$> mapM compileTxConstraint (byKind Out)

      return $
        MkResolvedTx
          { txIns = witnesedScriptTxIns <> map fst txInsPairs
          , txInsReference = []
          , txOuts
          , toMint = TxMintNone
          , additionalSigners = signers scriptTransition
          , signer = error "TODO"
          , interval = always
          }
    where
      script = cemScriptCompiled (Proxy :: Proxy script)
      scriptAddress = cemScriptAddress (Proxy :: Proxy script)
      resolveTxIn (MkTxFanC _ (MkTxFanFilter addressSpec _) _) = do
        utxo <- lift $ queryUtxo $ ByAddresses [address]
        return $ map (\(x, y) -> (withKeyWitness x, y)) $ Map.toList $ unUTxO utxo
        where
          address = addressSpecToAddress scriptAddress addressSpec
      compileTxConstraint
        (MkTxFanC _ (MkTxFanFilter addressSpec filterSpec) quantor) = do
          address' <- lift $ fromPlutusAddressInMonad address
          let compiledTxOut value =
                TxOut address' value datum ReferenceScriptNone
          return $ case quantor of
            Exist n -> replicate (fromInteger n) $ compiledTxOut minUtxoValue
            SumValueEq value -> [compiledTxOut $ (convertTxOut $ fromPlutusValue value) <> minUtxoValue]
          where
            datum = case filterSpec of
              Anything -> TxOutDatumNone
              ByDatum datum' -> mkInlineDatum datum'
              -- FIXME: Can be optimized via Plutarch
              UnsafeBySameCEM newState ->
                let
                  cemDatum :: CEMScriptDatum script
                  cemDatum =
                    ( stagesParams params
                    , scriptParams params
                    , unsafeFromBuiltinData newState
                    )
                 in
                  mkInlineDatum cemDatum
            address = addressSpecToAddress scriptAddress addressSpec
            -- TODO: protocol params
            -- calculateMinimumUTxO era txout bpp
            minUtxoValue = convertTxOut $ lovelaceToValue 3_000_000
            -- TODO
            convertTxOut x =
              TxOutValueShelleyBased shelleyBasedEra $ toMaryValue x

resolveTx ::
  (MonadQueryUtxo m, MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (Either TxResolutionError ResolvedTx)
resolveTx spec = runExceptT $ do
  -- Get specs
  !actionsSpecs <- mapM (ExceptT . resolveAction) $ actions spec

  -- Merge specs
  let
    mergedSpec' = head actionsSpecs
    mergedSpec = (mergedSpec' :: ResolvedTx) {signer = specSigner spec}

  return mergedSpec

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
