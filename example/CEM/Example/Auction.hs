{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | CEM Script Acution example -- simple no-deposit acutons
module CEM.Example.Auction where

import Cardano.CEM
import Data.Map qualified as Map
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)
import PlutusTx.Prelude
import Prelude qualified

-- | Tag
data SimpleAuction

-- | A bid
data Bid = MkBet
  { better :: PubKeyHash
  , betAmount :: Integer
  }
  deriving stock (Prelude.Eq, Prelude.Show)

{- | 'Bid' is the only type we need to derive HasPlutusSpine intance,
since it's not directly referenced from 'CEMScript'.
-}
derivePlutusSpine ''Bid

data SimpleAuctionState
  = NotStarted
  | CurrentBid
      { bid :: Bid
      }
  | Winner
      { bid :: Bid
      }
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionParams = MkAuctionParams
  { seller :: PubKeyHash
  , lot :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionTransition
  = Create
  | Start
  | MakeBid
      { bid :: Bid
      }
  | Close
  | Buyout
  deriving stock (Prelude.Eq, Prelude.Show)

instance CEMScriptTypes SimpleAuction where
  type Params SimpleAuction = SimpleAuctionParams
  type State SimpleAuction = SimpleAuctionState
  type Transition SimpleAuction = SimpleAuctionTransition

$(deriveCEMAssociatedTypes False ''SimpleAuction)

instance CEMScript SimpleAuction where
  compilationConfig = MkCompilationConfig "AUC"

  perTransitionScriptSpec =
    let
      buyoutBid = ctxState.bid

      initialBid =
        cOfSpine
          MkBetSpine
          [ #better ::= ctxParams.seller
          , #betAmount ::= lift 0
          ]

      auctionValue = cMinLovelace @<> ctxParams.lot
     in
      Map.fromList
        [
          ( CreateSpine
          ,
            [ spentBy ctxParams.seller cMinLovelace cEmptyValue
            , output (ownUtxo $ withNullaryState NotStartedSpine) auctionValue
            ]
          )
        ,
          ( StartSpine
          ,
            [ input (ownUtxo $ inState NotStartedSpine) auctionValue
            , output (ownUtxo $ withState CurrentBidSpine [#bid ::= initialBid]) auctionValue
            , signedBy ctxParams.seller
            ]
          )
        ,
          ( MakeBidSpine
          ,
            [ input (ownUtxo $ inState CurrentBidSpine) auctionValue
            , byFlagError
                (ctxTransition.bid.betAmount @<= ctxState.bid.betAmount)
                "Bid amount is less or equal to current bid"
            , output
                ( ownUtxo
                    $ withState
                      CurrentBidSpine
                      [#bid ::= ctxTransition.bid]
                )
                auctionValue
            , signedBy ctxTransition.bid.better
            ]
          )
        ,
          ( CloseSpine
          ,
            [ input (ownUtxo $ inState CurrentBidSpine) auctionValue
            , output
                ( ownUtxo
                    $ withState WinnerSpine [#bid ::= ctxState.bid]
                )
                auctionValue
            , signedBy ctxParams.seller
            ]
          )
        ,
          ( BuyoutSpine
          ,
            [ input (ownUtxo $ inState WinnerSpine) auctionValue
            , -- Example: In constraints redundant for on-chain
              offchainOnly
                ( spentBy
                    buyoutBid.better
                    ( cMkAdaOnlyValue buyoutBid.betAmount
                        @<> cMinLovelace
                    )
                    cEmptyValue
                )
            , output
                (userUtxo ctxParams.seller)
                (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.betAmount)
            , output
                (userUtxo buyoutBid.better)
                (cMinLovelace @<> ctxParams.lot)
            ]
          )
        ]
