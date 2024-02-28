module Main (main) where

import Prelude hiding (readFile)

import Control.Monad.Trans
import Data.ByteString (putStr, readFile)
import System.Random

import Text.Show.Pretty

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (ReferenceScript (..), toMaryValue)

import Cardano.CEM
import Cardano.CEM.Examples.Compilation

-- import Cardano.CEM.Examples.Escrow
import Cardano.CEM.Monads
import Cardano.CEM.Monads.L1
import Cardano.CEM.Stages
import Cardano.Extras

import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Auction (SimpleAuctionState (CurrentBet, NotStarted))
import Cardano.CEM.Monads (queryScriptState)
import Cardano.Ledger.Val (adaOnly)
import Data.Map (elems, keys)
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Interval (always)
import PlutusLedgerApi.V1.Value (adaSymbol, adaToken, assetClass, assetClassValue)
import Test.Hspec (around, describe, hspec, it, shouldBe, shouldSatisfy)
import Unsafe.Coerce (unsafeCoerce)

data TestContext = MkTestContext
  { testEnvKeys :: [SigningKey PaymentKey]
  }

keysPaths =
  [ "./devnet/credentials/faucet.sk"
  , "./devnet/credentials/bob.sk"
  , "./devnet/credentials/carol.sk"
  ]

readTestContext :: IO (TestContext)
readTestContext = do
  testEnvKeys <- mapM readKey keysPaths
  return (MkTestContext {testEnvKeys})
  where
    readKey path = do
      Just key <- liftIO $ parseSigningKeyTE <$> readFile path
      return key

withContext :: (TestContext -> IO ()) -> IO ()
withContext action = do
  context <- readTestContext
  action context

checkTxCreated ::
  (MonadQueryUtxo m, MonadIO m) => TxId -> m ()
checkTxCreated txId = do
  -- TODO: better out checks
  awaitTx txId
  let
    txIn = TxIn txId (TxIx 0)
    someValue = lovelaceToValue $ fromInteger 0
  utxo <- queryUtxo $ ByTxIns [txIn]
  liftIO $ shouldSatisfy (utxoValue utxo) (/= someValue)

submitAndCheck spec = do
  case head $ actions spec of
    MkSomeCEMAction (MkCEMAction _ transition) ->
      liftIO $ putStrLn $ "Doing " <> show transition
  result <- resolveTxAndSubmit spec
  case result of
    Right txId -> do
      awaitTx txId
      liftIO $ putStrLn $ "Awaited " <> show txId
    Left error -> fail $ show error

main :: IO ()
main = hspec $ around withContext $ do
  describe "Checking monad works" $ do
    it "Asking NetworkId works" $ \_context -> execOnLocalDevnet $ do
      networkId <- askNetworkId
      liftIO $ networkId `shouldBe` localDevnetNetworkId
    it "Querying blockchain params works" $ \_context -> execOnLocalDevnet $ do
      _slotNo <- queryCurrentSlot
      _blockchainParams <- queryBlockchainParams
      return ()
    it "Querying UTxO works" $ \context -> execOnLocalDevnet $ do
      utxo <-
        queryUtxo $
          ByAddresses
            [ signingKeyToAddress $ testEnvKeys context !! 0
            ]
      return ()
    it "Sending transaction works" $ \context -> execOnLocalDevnet $ do
      utxo <-
        queryUtxo $
          ByAddresses
            [ signingKeyToAddress $ testEnvKeys context !! 0
            ]

      user1Address <-
        fromPlutusAddressInMonad $ signingKeyToAddress $ testEnvKeys context !! 0
      user2Address <-
        fromPlutusAddressInMonad $ signingKeyToAddress $ testEnvKeys context !! 1
      let
        user1TxIns = keys $ unUTxO utxo
        Just value = valueToLovelace $ utxoValue utxo
        convert x =
          TxOutValueShelleyBased shelleyBasedEra $
            toMaryValue x
        out userAddress =
          TxOut
            userAddress
            (convert $ lovelaceToValue $ fromInteger 10_000_000)
            TxOutDatumNone
            ReferenceScriptNone
        tx =
          MkResolvedTx
            { txIns = map withKeyWitness user1TxIns
            , txInsReference = []
            , txOuts =
                [ out user1Address
                , out user2Address
                ]
            , toMint = TxMintNone
            , interval = always
            , signors = [testEnvKeys context !! 0]
            }
      Right txId <- submitResolvedTx tx
      checkTxCreated txId

      return ()

  describe "SimpleAuction usecase" $ do
    it "Wrong transition resolution error" $ \context -> execOnLocalDevnet $ do
      let
        seller = testEnvKeys context !! 0
        bidder1 = testEnvKeys context !! 1
        auctionParams =
          MkCEMParams
            { scriptParams =
                MkAuctionParams
                  { seller = signingKeyToPKH seller
                  , lot =
                      assetClassValue
                        (assetClass adaSymbol adaToken)
                        10_000_000
                  }
            , stagesParams = NoControl
            }

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $ MkCEMAction auctionParams Create
              ]
          , specSigners = [mkMainSigner seller]
          }

      let
        bid1 =
          MkBet
            { better = signingKeyToPKH bidder1
            , betAmount = 1_000_000
            }

      result <-
        resolveTxAndSubmit $
          MkTxSpec
            { actions =
                [ MkSomeCEMAction $
                    MkCEMAction auctionParams (MakeBet bid1)
                ]
            , specSigners = [mkMainSigner bidder1]
            }
      Left
        ( MkTransitionError
            _
            (StateMachineError "\"Incorrect state for transition\"")
          ) <-
        return result

      return ()

    it "Wrong bid resolution error" $ \context -> execOnLocalDevnet $ do
      let
        seller = testEnvKeys context !! 0
        bidder1 = testEnvKeys context !! 1
        auctionParams =
          MkCEMParams
            { scriptParams =
                MkAuctionParams
                  { seller = signingKeyToPKH seller
                  , lot =
                      assetClassValue
                        (assetClass adaSymbol adaToken)
                        10_000_000
                  }
            , stagesParams = NoControl
            }

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $ MkCEMAction auctionParams Create
              ]
          , specSigners = [mkMainSigner seller]
          }

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams Start
              ]
          , specSigners = [mkMainSigner seller]
          }

      let
        bid1 =
          MkBet
            { better = signingKeyToPKH bidder1
            , betAmount = 0
            }

      result <-
        resolveTxAndSubmit $
          MkTxSpec
            { actions =
                [ MkSomeCEMAction $
                    MkCEMAction auctionParams (MakeBet bid1)
                ]
            , specSigners = [mkMainSigner bidder1]
            }
      Left
        ( MkTransitionError
            _
            (StateMachineError "\"Incorrect state for transition\"")
          ) <-
        return result

      return ()

    it "Successful transition flow" $ \context -> execOnLocalDevnet $ do
      -- XXX: blockchain state is reused, so we need to differentiate Utxos
      paramJitter <- liftIO $ getStdRandom (randomR (0, 1_000_000))
      let
        seller = testEnvKeys context !! 0
        bidder1 = testEnvKeys context !! 1
        auctionParams =
          MkCEMParams
            { scriptParams =
                MkAuctionParams
                  { seller = signingKeyToPKH seller
                  , lot =
                      assetClassValue
                        (assetClass adaSymbol adaToken)
                        (10_000_000 + paramJitter)
                  }
            , stagesParams = NoControl
            }

      Nothing <- queryScriptState auctionParams

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $ MkCEMAction auctionParams Create
              ]
          , specSigners = [mkMainSigner seller]
          }

      Just NotStarted <- queryScriptState auctionParams

      let
        initBid =
          MkBet
            { better = signingKeyToPKH seller
            , betAmount = 0
            }
        bid1 =
          MkBet
            { better = signingKeyToPKH bidder1
            , betAmount = 3_000_000
            }

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams Start
              ]
          , specSigners = [mkMainSigner seller]
          }

      Just (CurrentBet currentBid') <- queryScriptState auctionParams
      liftIO $ currentBid' `shouldBe` initBid

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams (MakeBet bid1)
              ]
          , specSigners = [mkMainSigner bidder1]
          }

      Just (CurrentBet currentBid) <- queryScriptState auctionParams
      liftIO $ currentBid `shouldBe` bid1

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams Close
              ]
          , specSigners = [mkMainSigner seller]
          }

      submitAndCheck $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams (Buyout $ signingKeyToAddress bidder1)
              ]
          , specSigners = [mkMainSigner bidder1]
          }
