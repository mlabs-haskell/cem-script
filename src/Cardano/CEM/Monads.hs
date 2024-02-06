module Cardano.CEM.Monads where

import Prelude

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Set (Set)

-- import Control.Monad.Trans.Either (EitherT(..))

import PlutusLedgerApi.V1.Address (Address)
import PlutusLedgerApi.V2 (
  Interval (..),
  POSIXTime (..),
  UnsafeFromData (..),
  always,
  fromData,
 )

import Cardano.Api hiding (Address, In, Out, queryUtxo, txIns)
import Cardano.Api.Shelley (PlutusScript (..), PoolId, ReferenceScript (..), fromPlutusData, toMaryValue, toPlutusData)
import Cardano.Ledger.Core (PParams)

import Cardano.CEM
import Cardano.CEM.OnChain
import Cardano.Extras
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.Trans (MonadTrans (..))
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Spine (HasSpine (..))
import Debug.Trace (trace, traceM)
import Text.Show.Pretty (ppShow)

-- MonadBlockchainParams

-- @todo #13: Derive stock classes for cardano-api

-- | Params of blockchain required for transaction-building
data BlockchainParams = MkBlockchainParams
  { protocolParameters :: PParams LedgerEra
  , systemStart :: SystemStart
  , eraHistory :: EraHistory
  , stakePools :: Set PoolId
  }

{- | This monad gives access to all information about Cardano params,
 | which is various kind of Ledger params and ValidityBound/Slots semantics
-}

-- @todo #13: Implement PSM for `MonadBlockchainParams` (task for Michal)
class (MonadFail m) => MonadBlockchainParams m where
  askNetworkId :: m NetworkId
  queryCurrentSlot :: m SlotNo
  queryBlockchainParams :: m BlockchainParams

fromPlutusAddressInMonad ::
  (MonadBlockchainParams m) => Address -> m (AddressInEra Era)
fromPlutusAddressInMonad address = do
  networkId <- askNetworkId
  return $ fromPlutusAddress networkId address

-- MonadQuery

data UtxoQuery
  = ByAddresses [Address]
  | ByTxIns [TxIn]
  deriving stock (Show, Eq)

class (MonadBlockchainParams m) => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m (UTxO Era)

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

-- MonadSubmit

data ResolvedTx = MkResolvedTx
  { txIns :: [(TxIn, TxInWitness)]
  , txInsReference :: [TxIn]
  , txOuts :: [TxOut CtxTx Era]
  , toMint :: TxMintValue BuildTx Era
  , interval :: Interval POSIXTime
  , signors :: [SigningKey PaymentKey]
  }
  deriving stock (Show, Eq)

data WrongSlotKind = Early | Late
  deriving stock (Show, Eq)

data TxSubmittingError
  = WrongSlot WrongSlotKind Integer
  | TxInOutdated [TxIn]
  | UnhandledNodeError String
  deriving stock (Show, Eq)

class (MonadQueryUtxo m) => MonadSubmitTx m where
  submitResolvedTx :: ResolvedTx -> m (Either TxSubmittingError TxId)

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

deriving stock instance
  ( Show (CEMParams script)
  , Show (State script)
  , Show (Transition script)
  ) =>
  Show (CEMAction script)

-- deriving instance
--   ( Eq (State script)
--   , Eq (CEMParams script)
--   , Eq (Transition script)
--   ) =>
--   Eq (CEMAction script)

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

data TransitionError
  = StateMachineError
      { errorMessage :: String
      }
  | MissingTransitionInputh
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

      let
        byKind kind =
          filter (\x -> txFanCKind x == kind) $
            constraints scriptTransition

      txIns <- concat <$> mapM resolveTxIn (byKind In)
      txOuts <- concat <$> mapM compileTxConstraint (byKind Out)

      return $
        MkResolvedTx
          { txIns = witnesedScriptTxIns ++ txIns
          , txInsReference = []
          , txOuts
          , toMint = TxMintNone
          , signors = []
          , interval = always
          }
    where
      script = cemScriptCompiled (Proxy :: Proxy script)
      scriptAddress = cemScriptAddress (Proxy :: Proxy script)
      resolveTxIn (MkTxFanC _ (MkTxFanFilter addressSpec filterSpec) quantor) = do
        utxo <- lift $ queryUtxo $ ByAddresses [address]
        return $ map withKeyWitness $ Map.keys $ unUTxO utxo
        where
          address = addressSpecToAddress scriptAddress addressSpec
      compileTxConstraint
        (MkTxFanC _ (MkTxFanFilter addressSpec filterSpec) quantor) = do
          address' <- lift $ fromPlutusAddressInMonad address
          let compiledTxOut value =
                TxOut address' value datum ReferenceScriptNone
          return $ case quantor of
            Exist n -> replicate (fromInteger n) $ compiledTxOut minUtxoValue
            SumValueEq value -> [compiledTxOut $ convertTxOut $ fromPlutusValue value]
          where
            datum = case filterSpec of
              Anything -> TxOutDatumNone
              ByDatum datum' -> mkInlineDatum datum'
              BySameCEM newState ->
                let
                  datum :: CEMScriptDatum script
                  datum = (stagesParams params, scriptParams params, unsafeFromBuiltinData newState)
                 in
                  mkInlineDatum datum
            address = addressSpecToAddress scriptAddress addressSpec
            -- TODO: protocol params
            -- calculateMinimumUTxO era txout bpp
            minUtxoValue = convertTxOut $ lovelaceToValue $ Lovelace 2_000_000
            -- TODO
            convertTxOut x =
              TxOutValueShelleyBased shelleyBasedEra $ toMaryValue x

resolveTxAndSubmit ::
  (MonadQueryUtxo m, MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (Either TxResolutionError TxId)
resolveTxAndSubmit spec = runExceptT $ do
  -- Get specs
  actionsSpecs <- mapM (ExceptT . resolveAction) $ actions spec

  -- Merge specs
  let
    mergedSpec' = head actionsSpecs
    mergedSpec = mergedSpec' {signors = map signerKey $ specSigners spec}

  -- TODO
  utxo <- lift $ queryUtxo $ ByAddresses [signingKeyToAddress $ head $ signors mergedSpec]
  let ins =
        map withKeyWitness $ Map.keys $ unUTxO utxo

  result <- lift $ submitResolvedTx $ mergedSpec {txIns = (txIns mergedSpec) ++ ins}
  case result of
    Right txId -> return txId
    Left resolveError -> throwError $ UnhandledSubmittingError resolveError
