module Auction where

import Prelude

import Control.Monad.Trans (MonadIO (..))
import PlutusLedgerApi.V1.Value (assetClassValue)

import Cardano.CEM
import Cardano.CEM.Examples.Auction
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.Extras

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
    -- ~( Left
    --     ( MkTransitionError
    --         _
    --         (StateMachineError "\"Incorrect state for transition\"")
    --       )
    --   ) <-
    --   return result
    (Left _) <- return result

    return ()

  it "Successful transition flow" $ execClb $ do
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

    (tx, txInMode, utxo) <-
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

    -- liftIO $ print tx
    -- liftIO $ print txInMode
    liftIO $ print utxo

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
