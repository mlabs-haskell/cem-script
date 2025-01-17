{-# OPTIONS_GHC -Wno-orphans #-}

module CEM.Test.Dynamic where

import CEM.Example.Auction
import CEM.Example.Compiled ()
import CEM.Test.TestNFT (testNftAssetClass)
import CEM.Test.Utils (execClb, mintTestTokens)
import Cardano.Api (lovelaceToValue)
import Cardano.CEM (MonadTest (getTestWalletSks))
import Cardano.CEM.Testing.StateMachine (
  Action (SetupConfig, SetupParams),
  CEMScriptArbitrary (..),
  CEMScriptRunModel (..),
  ScriptState (ConfigSet),
  ScriptStateParams (config),
  TestConfig (MkTestConfig, actors, doMutationTesting),
  findSkForPKH,
  runActionsInClb,
 )
import Cardano.Extras (signingKeyToPKH)
import PlutusLedgerApi.V1.Value (assetClassValue)
import Test.Hspec (describe, it, shouldBe)
import Test.QuickCheck (
  Property,
  chooseInteger,
  elements,
  frequency,
  isSuccess,
  quickCheckResult,
  withMaxSuccess,
 )
import Test.QuickCheck.DynamicLogic (
  DL,
  action,
  anyActions_,
  forAllDL,
 )
import Prelude

-- Defining generic instances

instance CEMScriptArbitrary SimpleAuction where
  arbitraryParams actors = do
    seller <- elements actors
    return $
      MkAuctionParams
        { seller = signingKeyToPKH seller
        , lot = assetClassValue testNftAssetClass 1
        }

  arbitraryTransition dappParams state = case state of
    Nothing -> return Create
    Just NotStarted -> return Start
    Just (CurrentBid bid) ->
      frequency
        [
          ( 100
          , MakeBid <$> (MkBet <$> genBidder <*> genBid bid)
          )
        , (1, return Close)
        ]
    Just (Winner {}) -> return Buyout
    where
      genBidder = elements (map signingKeyToPKH $ actors $ config dappParams)
      genBid bid = (bidAmount bid +) <$> chooseInteger (0, 100_500)

instance CEMScriptRunModel SimpleAuction where
  performHook
    (ConfigSet (MkTestConfig {actors}))
    (SetupParams cemParams) = do
      let s = seller cemParams
      mintTestTokens (findSkForPKH actors s) 1
      return ()
  performHook _ _ = return ()

-- Run tests

dynamicSpec = describe "Quickcheck Dynamic" $ do
  it "Auction random trace works on CLB" $ do
    quickCheckDLScript $ do
      anyActions_
  where
    quickCheckDLScript :: DL (ScriptState SimpleAuction) () -> IO ()
    quickCheckDLScript dl = do
      actors <- execClb getTestWalletSks
      result <- quickCheckResult $ withMaxSuccess 100 $ runDLScript $ do
        _ <-
          action $
            SetupConfig $
              MkTestConfig
                { actors
                , doMutationTesting = True
                }
        dl
      isSuccess result `shouldBe` True

    runDLScript :: DL (ScriptState SimpleAuction) () -> Property
    runDLScript dl =
      forAllDL
        dl
        (runActionsInClb @SimpleAuction genesisValue)

    genesisValue = lovelaceToValue 300_000_000_000
