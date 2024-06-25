module Auction where

import Prelude

import Control.Monad.Trans (MonadIO (..))
import Data.Proxy (Proxy (..))
import GHC.IsList

import Text.Show.Pretty (ppShow)

import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.CEM.OnChain (CEMScriptCompiled (..))
import Cardano.Extras

import Plutarch.Script
import PlutusLedgerApi.V1.Value (assetClassValue)

import Test.Hspec (describe, it, shouldBe)

import TestNFT (testNftAssetClass)
import Utils (execClb, mintTestTokens, perTransitionStats, submitAndCheck)

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
    ~(Left _) <-
      -- ( MkTransitionError
      --     _
      --     (StateMachineError "\"Incorrect state for transition\"")
      --   )
      return result

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
    ~( Left
        _
      ) <-
      -- ( MkTransitionError
      --     _
      --     (StateMachineError "\"Incorrect state for transition\"")
      --   )
      return result

    return ()

  it "Successful transition flow" $ execClb $ do
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
        , specSigner = seller
        }

    Just (CurrentBid currentBid') <- queryScriptState auctionParams
    liftIO $ currentBid' `shouldBe` initBid

    submitAndCheck $
      MkTxSpec
        { actions =
            [ MkSomeCEMAction $
                MkCEMAction auctionParams (MakeBid bid1)
            ]
        , specSigner = bidder1
        }

    Just (CurrentBid currentBid) <- queryScriptState auctionParams
    liftIO $ currentBid `shouldBe` bid1

    submitAndCheck $
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
        , specSigner = bidder1
        }

    -- FIXME
    stats <- perTransitionStats
    liftIO $ putStrLn $ ppShow stats
