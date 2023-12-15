{- | User-facing utilities for querying and sending Txs
on top of interfaces in `Monads` module
-}
module Cardano.CEM.OffChain where

import Prelude

-- Haskell imports
import Control.Concurrent (threadDelay)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.List (find)
import Data.Map qualified as Map
import Data.Set (Set)

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  UnsafeFromData (..),
  always,
  fromData,
 )

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns)
import Cardano.Api.IPC (TxValidationError)
import Cardano.Api.Shelley (
  PlutusScript (..),
  PoolId,
  ReferenceScript (..),
  fromPlutusData,
  toMaryValue,
  toPlutusData,
 )
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Shelley.API (ApplyTxError)

-- Project imports

import Cardano.CEM
import Cardano.CEM.Monads
import Cardano.CEM.OnChain
import Cardano.Extras
import Data.Spine

fromPlutusAddressInMonad ::
  (MonadBlockchainParams m) => Address -> m (AddressInEra Era)
fromPlutusAddressInMonad address = do
  networkId <- askNetworkId
  return $ fromPlutusAddress networkId address

queryByFanFilter :: (MonadQueryUtxo m) => TxFanFilter script -> m (UTxO Era)
queryByFanFilter query = return $ error "TODO"

checkTxIdExists :: (MonadQueryUtxo m) => TxId -> m Bool
checkTxIdExists txId = do
  result <- queryUtxo $ ByTxIns [TxIn txId (TxIx 0)]
  return $ not $ Map.null $ unUTxO result

awaitTx :: forall m. (MonadIO m, MonadQueryUtxo m) => TxId -> m ()
awaitTx txId = do
  go 5
  where
    go :: Integer -> m ()
    go 0 = liftIO $ fail "Tx was not awaited." -- TODO
    go n = do
      exists <- checkTxIdExists txId
      liftIO $ threadDelay 1_000_000
      if exists
        then return ()
        else go $ n - 1

data TxSigner = MkTxSigner
  { signerKey :: SigningKey PaymentKey
  , allowTxInSpending :: Bool
  , allowFeeCovering :: Bool
  }
  deriving stock (Show, Eq)

mkMainSigner :: SigningKey PaymentKey -> TxSigner
mkMainSigner signerKey =
  MkTxSigner
    { signerKey
    , allowTxInSpending = True
    , allowFeeCovering = True
    }

data CEMAction script
  = MkCEMAction (CEMParams script) (Transition script)

-- TODO
deriving stock instance
  ( Show (CEMParams script)
  , Show (State script)
  , Show (Transition script)
  ) =>
  Show (CEMAction script)

data SomeCEMAction where
  MkSomeCEMAction ::
    forall script.
    ( CEMScriptCompiled script
    , Show (CEMAction script)
    , Show (State script)
    , Show (Transition script)
    , Eq (CEMParams script)
    ) =>
    CEMAction script ->
    SomeCEMAction

instance Show SomeCEMAction where
  -- TODO: show script name
  show :: SomeCEMAction -> String
  show (MkSomeCEMAction action) = show action

data TxSpec = MkTxSpec
  { actions :: [SomeCEMAction]
  , specSigners :: [TxSigner]
  }
  deriving stock (Show)

-- | Error occurred while trying to execute CEMScript transition
data TransitionError
  = StateMachineError
      { errorMessage :: String
      }
  | MissingTransitionInput
  deriving stock (Show, Eq)

data TxResolutionError
  = TxSpecIsIncorrect
  | MkTransitionError SomeCEMAction TransitionError
  | UnhandledSubmittingError TxSubmittingError
  deriving stock (Show)

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
  , Eq (CEMParams script)
  ) =>
  CEMParams script ->
  m (Maybe (TxIn, TxOut CtxUTxO Era))
queryScriptTxInOut params = do
  utxo <- queryUtxo $ ByAddresses [scriptAddress]
  let mScriptTxIn =
        case Map.assocs $ unUTxO utxo of
          [] -> Nothing
          pairs -> find hasSameParams pairs
      hasSameParams (txIn, txOut) =
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
  , Eq (CEMParams script)
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
          (_, Nothing) -> Nothing
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

      let
        txInValue = mconcat $ map (txOutValue . snd) txInsPairs
        txOutValue' = mconcat $ map txOutValue txOuts

      -- TODO
      -- traceM $
      --   "Doing transition: " <> ppShow someAction <>
      --   "From state: " <> ppShow mState <>
      --   "With transition spec: " <> ppShow scriptTransition
      -- traceM $ ppShow witnesedScriptTxIns
      -- traceM $ ppShow txInsPairs
      -- traceM $ ppShow txOutValue'

      return $
        MkResolvedTx
          { txIns = witnesedScriptTxIns <> map fst txInsPairs
          , txInsReference = []
          , txOuts
          , toMint = TxMintNone
          , signer = []
          , interval = always
          }
    where
      txOutValue (TxOut _ value _ _) = value
      script = cemScriptCompiled (Proxy :: Proxy script)
      scriptAddress = cemScriptAddress (Proxy :: Proxy script)
      resolveTxIn (MkTxFanC _ (MkTxFanFilter addressSpec filterSpec) quantor) = do
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
                  datum :: CEMScriptDatum script
                  datum = (stagesParams params, scriptParams params, unsafeFromBuiltinData newState)
                 in
                  mkInlineDatum datum
            address = addressSpecToAddress scriptAddress addressSpec
            -- TODO: protocol params
            -- calculateMinimumUTxO era txout bpp
            minUtxoValue = convertTxOut $ lovelaceToValue $ Lovelace 3_000_000
            -- TODO
            convertTxOut x =
              TxOutValueShelleyBased shelleyBasedEra $ toMaryValue x

resolveTxAndSubmit ::
  (MonadQueryUtxo m, MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (Either TxResolutionError TxId)
resolveTxAndSubmit spec = runExceptT $ do
  -- Get specs
  !actionsSpecs <- mapM (ExceptT . resolveAction) $ actions spec

  -- Merge specs
  let
    mergedSpec' = head actionsSpecs
    mergedSpec = mergedSpec' {signer = map signerKey $ specSigners spec}

  -- TODO
  !utxo <- lift $ queryUtxo $ ByAddresses [signingKeyToAddress $ head $ signer mergedSpec]
  let ins = map withKeyWitness $ Map.keys $ unUTxO utxo
  let result = submitResolvedTx $ mergedSpec {txIns = txIns mergedSpec ++ ins}
  ExceptT $ (bimap UnhandledSubmittingError id) <$> result
