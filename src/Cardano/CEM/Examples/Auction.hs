{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.CEM.Examples.Auction where

import PlutusTx.Prelude
import Prelude qualified

import Data.Map qualified as Map

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)

import Cardano.CEM
import Cardano.CEM.TH (deriveCEMAssociatedTypes)
import Data.Spine

-- Simple no-deposit auction

data SimpleAuction

data Bid = MkBet
  { better :: PubKeyHash
  , betAmount :: Integer
  }
  deriving stock (Prelude.Eq, Prelude.Show)

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
    Map.fromList
      [
        ( CreateSpine
        ,
          [ MainSignerCoinSelect ctxParams.seller cMinLovelace cEmptyValue
          , TxFan Out (SameScript $ Pure NotStarted) scriptStateValue
          ]
        )
      ,
        ( StartSpine
        ,
          [ sameScriptIncOfSpine NotStartedSpine
          , -- TxFan
            --   In
            --   (SameScript $ Pure NotStarted)
            --   scriptStateValue
            TxFan
              Out
              ( SameScript
                  $ cOfSpine CurrentBidSpine [#bid ::= initialBid]
              )
              scriptStateValue
          , MainSignerNoValue ctxParams.seller
          ]
        )
      ,
        ( MakeBidSpine
        ,
          [ sameScriptIncOfSpine CurrentBidSpine
          , byFlagError
              (ctxTransition.bid.betAmount @<= ctxState.bid.betAmount)
              "Bid amount is less or equal to current bid"
          , TxFan
              Out
              ( SameScript
                  $ cOfSpine
                    CurrentBidSpine
                    [#bid ::= ctxTransition.bid]
              )
              scriptStateValue
          , MainSignerNoValue ctxTransition.bid.better
          ]
        )
      ,
        ( CloseSpine
        ,
          [ sameScriptIncOfSpine CurrentBidSpine
          , TxFan
              Out
              ( SameScript
                  $ cOfSpine WinnerSpine [#bid ::= ctxState.bid]
              )
              scriptStateValue
          , MainSignerNoValue ctxParams.seller
          ]
        )
      ,
        ( BuyoutSpine
        ,
          [ sameScriptIncOfSpine WinnerSpine
          , -- Example: In constraints redundant for on-chain
            offchainOnly
              ( MainSignerCoinSelect
                  buyoutBid.better
                  ( cMkAdaOnlyValue buyoutBid.betAmount
                      @<> cMinLovelace
                  )
                  cEmptyValue
              )
          , TxFan
              Out
              (UserAddress ctxParams.seller)
              (cMinLovelace @<> cMkAdaOnlyValue buyoutBid.betAmount)
          , TxFan
              Out
              (UserAddress buyoutBid.better)
              (cMinLovelace @<> ctxParams.lot)
          ]
        )
      ]
    where
      buyoutBid = ctxState.bid
      initialBid =
        cOfSpine
          MkBetSpine
          [ #better ::= ctxParams.seller
          , #betAmount ::= Pure 0
          ]
      scriptStateValue = cMinLovelace @<> ctxParams.lot
      sameScriptIncOfSpine spine =
        TxFan
          In
          (SameScript $ cUpdateOfSpine ctxState spine [])
          scriptStateValue
