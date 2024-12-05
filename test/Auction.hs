{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Auction where

import Prelude

import Cardano.Api.NetworkId (toShelleyNetwork)
import Cardano.CEM
import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.Extras
import Control.Monad.Trans (MonadIO (..))
import OuraFilters.Mock (IndexerEvent (Following, Initial), extractEvent, resolvedTxToOura)
import PlutusLedgerApi.V1.Value (assetClassValue)
import Test.Hspec (describe, it, shouldBe)
import TestNFT (testNftAssetClass)
import Utils (execClb, mintTestTokens, submitAndCheck, submitCheckReturn)

auctionSpec = describe "Auction" $ do
  it "Wrong transition resolution error" $ execClb $ do
    seller <- (!! 0) <$> getTestWalletSks
    bidder1 <- (!! 1) <$> getTestWalletSks
    let
      auctionParams =
        MkCEMParams
          { scriptParams =
              MkAuctionParams
                { seller = signingKeyToPKH seller
                , lot =
                    assetClassValue
                      testNftAssetClass
                      1
                }
          , stagesParams = NoControl
          }

    mintTestTokens seller 1

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $ MkCEMAction auctionParams Create
            ]
        , specSigner = seller
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
                  MkCEMAction auctionParams (MakeBid bid1)
              ]
          , specSigner = bidder1
          }
    ( Left
        ( MkTransitionError
            _
            (StateMachineError "\"Incorrect state for transition\"")
          )
      ) <-
      return result

    return ()

  it "Wrong bid resolution error" $ execClb $ do
    seller <- (!! 0) <$> getTestWalletSks
    bidder1 <- (!! 1) <$> getTestWalletSks
    let
      auctionParams =
        MkCEMParams
          { scriptParams =
              MkAuctionParams
                { seller = signingKeyToPKH seller
                , lot =
                    assetClassValue
                      testNftAssetClass
                      10
                }
          , stagesParams = NoControl
          }

    mintTestTokens seller 10

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $ MkCEMAction auctionParams Create
            ]
        , specSigner = seller
        }

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $
                MkCEMAction auctionParams Start
            ]
        , specSigner = seller
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
                  MkCEMAction auctionParams (MakeBid bid1)
              ]
          , specSigner = bidder1
          }
    (Left _) <- return result

    return ()

  it "Successful transition flow" $ execClb $ do
    network <- toShelleyNetwork <$> askNetworkId
    seller <- (!! 0) <$> getTestWalletSks
    bidder1 <- (!! 1) <$> getTestWalletSks

    let
      auctionParams =
        MkCEMParams
          { scriptParams =
              MkAuctionParams
                { seller = signingKeyToPKH seller
                , lot =
                    assetClassValue
                      testNftAssetClass
                      10
                }
          , stagesParams = NoControl
          }

    mintTestTokens seller 10

    Nothing <- queryScriptState auctionParams

    (preBody, utxo) <-
      submitCheckReturn $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $ MkCEMAction auctionParams Create
              ]
          , specSigner = seller
          }

    Just NotStarted <- queryScriptState auctionParams

    mEvent <- liftIO $ extractEvent @SimpleAuction network $ resolvedTxToOura preBody utxo
    liftIO $ mEvent `shouldBe` Just (Initial CreateSpine)

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
      bid2 =
        MkBet
          { better = signingKeyToPKH bidder1
          , betAmount = 4_000_000
          }

    (preBody, utxo) <-
      submitCheckReturn $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams Start
              ]
          , specSigner = seller
          }

    Just (CurrentBid currentBid') <- queryScriptState auctionParams
    liftIO $ currentBid' `shouldBe` initBid

    mEvent <- liftIO $ extractEvent @SimpleAuction network $ resolvedTxToOura preBody utxo
    liftIO $ mEvent `shouldBe` Just (Following StartSpine)

    (preBody, utxo) <-
      submitCheckReturn $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams (MakeBid bid1)
              ]
          , specSigner = bidder1
          }

    Just (CurrentBid currentBid) <- queryScriptState auctionParams
    liftIO $ currentBid `shouldBe` bid1

    mEvent <- liftIO $ extractEvent @SimpleAuction network $ resolvedTxToOura preBody utxo
    liftIO $ mEvent `shouldBe` Just (Following MakeBidSpine)

    (preBody, utxo) <-
      submitCheckReturn $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction
                    auctionParams
                    ( MakeBid $ MkBet (signingKeyToPKH bidder1) 4_000_000
                    )
              ]
          , specSigner = bidder1
          }

    Just (CurrentBid currentBid) <- queryScriptState auctionParams
    liftIO $ currentBid `shouldBe` bid2

    mEvent <- liftIO $ extractEvent @SimpleAuction network $ resolvedTxToOura preBody utxo
    liftIO $ mEvent `shouldBe` Just (Following MakeBidSpine)

    (preBody, utxo) <-
      submitCheckReturn $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams Close
              ]
          , specSigner = seller
          }

    mEvent <- liftIO $ extractEvent @SimpleAuction network $ resolvedTxToOura preBody utxo
    liftIO $ mEvent `shouldBe` Just (Following CloseSpine)

    (preBody, utxo) <-
      submitCheckReturn $
        MkTxSpec
          { actions =
              [ MkSomeCEMAction $
                  MkCEMAction auctionParams Buyout
              ]
          , specSigner = bidder1
          }

    mEvent <- liftIO $ extractEvent @SimpleAuction network $ resolvedTxToOura preBody utxo
    liftIO $ mEvent `shouldBe` Just (Following BuyoutSpine)
