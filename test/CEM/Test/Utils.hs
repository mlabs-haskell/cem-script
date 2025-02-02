{-# LANGUAGE BlockArguments #-}

module CEM.Test.Utils where

import Prelude

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
 )
import Cardano.CEM (
  BlockchainMonadEvent (..),
  CEMAction (..),
  ClbRunner,
  Fees (..),
  MonadBlockchainParams (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  ResolvedTx (..),
  SomeCEMAction (..),
  TxSpec (..),
  UtxoQuery (..),
  awaitTx,
  execOnIsolatedClb,
  fromPlutusAddressInMonad,
  resolveTxAndSubmit,
  resolveTxAndSubmitRet,
  submitResolvedTx,
 )

import CEM.Test.TestNFT
import Cardano.Extras (
  Era,
  fromPlutusValue,
  mintedTokens,
  signingKeyToAddress,
  tokenToAsset,
  utxoValue,
  withKeyWitness,
 )
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (traverse_)
import Data.IORef qualified as IORef
import Data.Map (keys)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Spine (HasSpine (..))
import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V1.Value (assetClassValue)
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)
import System.Process qualified as Process
import System.Timeout (timeout)
import Test.Hspec (shouldSatisfy)
import Test.Hspec qualified as Hspec
import Text.Show.Pretty (ppShow)

withTimeout :: (Hspec.HasCallStack) => Float -> IO a -> IO a
withTimeout sec =
  maybe (error "Failed by timeout") pure
    <=< timeout (round $ sec * 10 ** 6)

resultToEither :: Aeson.Result a -> Either String a
resultToEither (Aeson.Success a) = Right a
resultToEither (Aeson.Error err) = Left err

execClb :: ClbRunner a -> IO a
execClb = execOnIsolatedClb $ lovelaceToValue 300_000_000

mintTestTokens ::
  (MonadIO m, MonadSubmitTx m) => SigningKey PaymentKey -> Integer -> m ()
mintTestTokens userSk numMint = do
  userAddress <- fromPlutusAddressInMonad $ signingKeyToAddress userSk
  utxo <- queryUtxo $ ByAddresses [signingKeyToAddress userSk]

  let
    user1TxIns = keys $ unUTxO utxo
    convert x =
      TxOutValueShelleyBased shelleyBasedEra $
        toMaryValue x
    out =
      TxOut
        userAddress
        ( convert $
            fromPlutusValue
              ( assetClassValue
                  testNftAssetClass
                  numMint
              )
              <> lovelaceToValue 3_000_000
        )
        TxOutDatumNone
        ReferenceScriptNone
    tx =
      MkResolvedTx
        { txIns = map withKeyWitness user1TxIns
        , txInRefs = []
        , txOuts = [out]
        , toMint =
            mintedTokens
              (PlutusScriptSerialised testNftPolicy)
              ()
              [(tokenToAsset testNftTokenName, fromInteger numMint)]
        , interval = always
        , additionalSigners = []
        , signer = userSk
        }
  awaitEitherTx =<< submitResolvedTx tx
  return ()

checkTxCreated ::
  (MonadQueryUtxo m, MonadIO m) => TxId -> m ()
checkTxCreated txId = do
  awaitTx txId
  let
    txIn = TxIn txId (TxIx 0)
    someValue = lovelaceToValue 0
  utxo <- queryUtxo $ ByTxIns [txIn]
  liftIO $ shouldSatisfy (utxoValue utxo) (/= someValue)

awaitEitherTx ::
  (MonadQueryUtxo m, MonadIO m, Show error) => Either error TxId -> m ()
awaitEitherTx eitherTx =
  case eitherTx of
    Right txId -> do
      awaitTx txId
    Left errorMsg -> error $ "Failed to send tx: " <> ppShow errorMsg

submitAndCheck :: (MonadSubmitTx m, MonadIO m) => TxSpec -> m ()
submitAndCheck spec = do
  case head $ actions spec of
    MkSomeCEMAction (MkCEMAction _ transition) ->
      liftIO $ putStrLn $ "  --> " <> show transition
  awaitEitherTx =<< resolveTxAndSubmit spec

submitCheckReturn ::
  (MonadSubmitTx m, MonadIO m) =>
  TxSpec ->
  m (TxBodyContent BuildTx Era, UTxO Era)
submitCheckReturn spec = do
  case head $ actions spec of
    MkSomeCEMAction (MkCEMAction _ transition) ->
      liftIO $ putStrLn $ "  --> " <> show transition
  ~(Right (tbc, tb, _, utxo)) <- resolveTxAndSubmitRet spec
  awaitTx $ getTxId tb
  pure (tbc, utxo)

perTransitionStats :: (MonadBlockchainParams m) => m (Map.Map String Fees)
perTransitionStats = do
  events <- eventList
  let feesByTxId = Map.fromList $ mapMaybe txIdFeePair events
  return $ Map.fromList $ mapMaybe (transitionFeePair feesByTxId) events
  where
    txIdFeePair (UserSpentFee {fees, txId}) = Just (txId, fees)
    txIdFeePair _ = Nothing
    transitionFeePair :: Map.Map TxId Fees -> BlockchainMonadEvent -> Maybe (String, Fees)
    transitionFeePair feesByTxId event = case event of
      ( SubmittedTxSpec
          (MkTxSpec [MkSomeCEMAction (MkCEMAction _ transition)] _)
          (Right txId)
        ) ->
          Just (show (getSpine transition), feesByTxId Map.! txId)
      _ -> Nothing

withNewFile :: String -> FilePath -> (FilePath -> IO a) -> IO a
withNewFile name dir action = do
  bracket
    ( do
        (path, handle) <- openTempFile dir name
        hClose handle
        pure path
    )
    removeFile
    action

testLogsFile :: FilePath
testLogsFile = "./test.log"

clearLogs :: IO ()
clearLogs = writeFile testLogsFile ""

debug :: String -> IO ()
debug msg = do
  appendFile testLogsFile $ msg <> "\n"

-- * Janitor

newtype SpotGarbage m a = MkSpotGarbage
  {run :: a -> m a}

data Janitor m a = MkJanitor
  { cleanup :: m ()
  , spotGarbage :: SpotGarbage m a
  }

-- Note, that the cleanup process may fail in this implementation
newJanitor ::
  forall a.
  (a -> IO ()) ->
  IO (Janitor IO a)
newJanitor trash = do
  spottedGarbageRef <- IORef.newIORef []
  pure
    MkJanitor
      { spotGarbage = MkSpotGarbage \garbage -> do
          IORef.modifyIORef spottedGarbageRef (garbage :)
          pure garbage
      , cleanup =
          IORef.readIORef spottedGarbageRef
            >>= traverse_ trash
      }

cleanupAround ::
  (a -> IO ()) ->
  (Janitor IO a -> b) ->
  Hspec.SpecWith b ->
  Hspec.Spec
cleanupAround trash toTestArg =
  Hspec.around $
    bracket (newJanitor trash) cleanup
      . flip fmap toTestArg

{- | This is a workaround the bug that cabal test can't finish before all its spawned processes
no matter what.
The workaround is to collect process handles and terminate them on error, allowing
test suite to finish.
-}
killProcessesOnError ::
  Hspec.SpecWith (SpotGarbage IO Process.ProcessHandle) ->
  Hspec.Spec
killProcessesOnError =
  cleanupAround Process.terminateProcess spotGarbage
