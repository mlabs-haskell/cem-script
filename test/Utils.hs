{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}

module Utils where

import Prelude

import Data.Map (keys)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)

import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V1.Value (assetClassValue)

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (
  PlutusScript (..),
  ReferenceScript (..),
  toMaryValue,
 )

import Test.Hspec (shouldSatisfy)
import Text.Show.Pretty (ppShow)

import Cardano.CEM.Monads (
  BlockchainMonadEvent (..),
  CEMAction (..),
  Fees (..),
  MonadBlockchainParams (..),
  MonadQueryUtxo (..),
  MonadSubmitTx (..),
  ResolvedTx (..),
  SomeCEMAction (..),
  TxSpec (..),
  UtxoQuery (..),
  submitResolvedTx,
 )
import Cardano.CEM.Monads.CLB (ClbRunner, execOnIsolatedClb)
import Cardano.CEM.OffChain (
  awaitTx,
  fromPlutusAddressInMonad,
  resolveTxAndSubmit,
 )
import Cardano.Extras
import Data.Spine (HasSpine (..))

import Control.Exception (bracket)
import Data.Foldable (traverse_)
import Data.IORef qualified as IORef
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)
import System.Process qualified as Process
import Test.Hspec qualified as Hspec
import TestNFT

totalDigits :: forall n m. (Integral n, RealFrac m, Floating m) => n -> n -> n
totalDigits base = round @m . logBase (fromIntegral base) . fromIntegral

digits :: forall n m. (Integral n, RealFrac m, Floating m) => n -> n -> [n]
digits base n =
  fst <$> case reverse [0 .. totalDigits @n @m base n - 1] of
    (i : is) ->
      scanl
        (\(_, remainder) digit -> remainder `divMod` (base ^ digit))
        (n `divMod` (base ^ i))
        is
    [] -> []

execClb :: ClbRunner a -> IO a
execClb = execOnIsolatedClb $ lovelaceToValue $ fromInteger 300_000_000

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
            ( fromPlutusValue $
                assetClassValue
                  testNftAssetClass
                  numMint
            )
              <> (lovelaceToValue $ fromInteger 3_000_000)
        )
        TxOutDatumNone
        ReferenceScriptNone
    tx =
      MkResolvedTx
        { txIns = map withKeyWitness user1TxIns
        , txInsReference = []
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
  -- TODO: better out checks
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
    -- liftIO $ putStrLn $ "Awaited " <> show txId
    Left errorMsg -> error $ "Failed to send tx: " <> ppShow errorMsg

submitAndCheck :: (MonadSubmitTx m, MonadIO m) => TxSpec -> m ()
submitAndCheck spec = do
  case head $ actions spec of
    MkSomeCEMAction (MkCEMAction _ transition) ->
      liftIO $ putStrLn $ "Doing " <> show transition
  awaitEitherTx =<< resolveTxAndSubmit spec

perTransitionStats :: (MonadBlockchainParams m) => m (Map.Map String Fees)
perTransitionStats = do
  events <- eventList
  let feesByTxId = Map.fromList $ mapMaybe txIdFeePair events
  return $ Map.fromList $ mapMaybe (transitionFeePair feesByTxId) events
  where
    txIdFeePair (UserSpentFee {fees, txId}) = Just (txId, fees)
    txIdFeePair _ = Nothing
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
