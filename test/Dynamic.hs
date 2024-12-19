{-# OPTIONS_GHC -Wno-orphans #-}

module Dynamic where

import Prelude

import PlutusLedgerApi.V1.Value (assetClassValue)

import Cardano.Api (lovelaceToValue)

import Test.Hspec (describe, it, shouldBe)
import Test.QuickCheck
import Test.QuickCheck.DynamicLogic

import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Monads (MonadTest (..))
import Cardano.CEM.Testing.StateMachine
import Cardano.Extras (signingKeyToPKH)

import TestNFT (testNftAssetClass)
import Utils (execClb, mintTestTokens)

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
      genBid bid = (betAmount bid +) <$> chooseInteger (0, 100_500)

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
    genesisValue = lovelaceToValue 300_000_000_000
    runDLScript dl =
      forAllDL
        dl
        (runActionsInClb @SimpleAuction genesisValue)
    quickCheckDLScript dl = do
      actors <- execClb getTestWalletSks
      result <- quickCheckResult $ runDLScript $ do
        _ <-
          action $
            SetupConfig $
              MkTestConfig
                { actors
                , doMutationTesting = True
                }
        dl
      isSuccess result `shouldBe` True
