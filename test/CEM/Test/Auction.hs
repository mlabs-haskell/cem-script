{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CEM.Test.Auction where

import CEM.Example.Auction
import CEM.Example.Compiled ()
import CEM.Test.TestNFT (testNftAssetClass)
import CEM.Test.Utils (
  execClb,
  mintTestTokens,
  submitAndCheck,
  submitCheckReturn,
 )
import Cardano.Api.NetworkId (toShelleyNetwork)
import Cardano.CEM
import Cardano.CEM.Indexing
import Cardano.Extras
import Control.Monad.Trans (MonadIO (..))
import Data.Proxy (Proxy (..))
import GHC.IsList
import Plutarch.Script
import PlutusLedgerApi.V1.Value (assetClassValue)
import Test.Hspec (describe, it, shouldBe)
import Prelude

-- import Text.Show.Pretty (ppShow)

auctionSpec = describe "AuctionSpec" $ do
  it "Serialise" $ do
    let script = cemScriptCompiled (Proxy :: Proxy SimpleAuction)
    putStrLn $
      "Script bytes: "
        <> show (length $ toList $ serialiseScript script)
  it "Wrong transition resolution error" $ execClb $ do
    seller <- (!! 0) <$> getTestWalletSks
    bidder1 <- (!! 1) <$> getTestWalletSks

    let
      auctionParams =
        MkAuctionParams
          { seller = signingKeyToPKH seller
          , lot =
              assetClassValue
                testNftAssetClass
                1
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
          { bidder = signingKeyToPKH bidder1
          , bidAmount = 1_000_000
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

    Left CEMScriptTxInResolutionError <- return result

    return ()

  it "Wrong bid resolution error" $ execClb $ do
    seller <- (!! 0) <$> getTestWalletSks
    bidder1 <- (!! 1) <$> getTestWalletSks
    let
      auctionParams =
        MkAuctionParams
          { seller = signingKeyToPKH seller
          , lot =
              assetClassValue
                testNftAssetClass
                10
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
          { bidder = signingKeyToPKH bidder1
          , bidAmount = 0
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
        MkAuctionParams
          { seller = signingKeyToPKH seller
          , lot =
              assetClassValue
                testNftAssetClass
                10
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
          { bidder = signingKeyToPKH seller
          , bidAmount = 0
          }
      bid1 =
        MkBet
          { bidder = signingKeyToPKH bidder1
          , bidAmount = 3_000_000
          }
      bid2 =
        MkBet
          { bidder = signingKeyToPKH bidder1
          , bidAmount = 4_000_000
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

  -- stats <- perTransitionStats
  -- liftIO $ putStrLn $ ppShow stats

  it "Zero bids flow" $ execClb $ do
    seller <- (!! 0) <$> getTestWalletSks

    let
      auctionParams =
        MkAuctionParams
          { seller = signingKeyToPKH seller
          , lot =
              assetClassValue
                testNftAssetClass
                10
          }

    mintTestTokens seller 10

    Nothing <- queryScriptState auctionParams

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $ MkCEMAction auctionParams Create
            ]
        , specSigner = seller
        }

    Just NotStarted <- queryScriptState auctionParams

    let
      initBid =
        MkBet
          { bidder = signingKeyToPKH seller
          , bidAmount = 0
          }

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $
                MkCEMAction auctionParams Start
            ]
        , specSigner = seller
        }

    Just (CurrentBid currentBid') <- queryScriptState auctionParams
    liftIO $ currentBid' `shouldBe` initBid

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $
                MkCEMAction auctionParams Close
            ]
        , specSigner = seller
        }

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $
                MkCEMAction auctionParams Buyout
            ]
        , specSigner = seller
        }

-- stats <- perTransitionStats
-- liftIO $ putStrLn $ ppShow stats
