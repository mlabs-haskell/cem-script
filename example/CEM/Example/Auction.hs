{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | CEM Script Acution example
module CEM.Example.Auction where

import Cardano.CEM
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)
import PlutusTx.Prelude
import Prelude qualified

-- | Simple no-deposit auction
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

  -- transitionStage _ =
  --   Map.fromList
  --     [ (CreateSpine, (Nothing, Just NotStartedSpine))
  --     , (StartSpine, (Just NotStartedSpine, Just CurrentBidSpine))
  --     , (MakeBidSpine, (Just CurrentBidSpine, Just CurrentBidSpine))
  --     , (CloseSpine, (Just CurrentBidSpine, Just WinnerSpine))
  --     , (BuyoutSpine, (Just WinnerSpine, Nothing))
  --     ]

  perTransitionScriptSpec =
    Map.fromList
      [
        ( CreateSpine
        ,
          [ MainSignerCoinSelect ctxParams.seller cMinLovelace cEmptyValue
          , -- , TxFan Out (SameScript $ MkSameScriptArg ctxState) scriptStateValue
            TxFan Out (SameScript $ MkSameScriptArg $ nullarySpine @SimpleAuctionState NotStartedSpine) scriptStateValue
          ]
        )
      ,
        ( StartSpine
        ,
          [ ownInputInState NotStartedSpine
          , TxFan
              Out
              ( SameScript
                  $ MkSameScriptArg
                  $ cOfSpine CurrentBidSpine [#bid ::= initialBid]
              )
              scriptStateValue
          , MainSignerNoValue ctxParams.seller
          ]
        )
      ,
        ( MakeBidSpine
        ,
          [ ownInputInState CurrentBidSpine
          , byFlagError
              (ctxTransition.bid.betAmount @<= ctxState.bid.betAmount)
              "Bid amount is less or equal to current bid"
          , TxFan
              Out
              ( SameScript
                  $ MkSameScriptArg
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
          [ ownInputInState CurrentBidSpine
          , TxFan
              Out
              ( SameScript
                  $ MkSameScriptArg
                  $ cOfSpine WinnerSpine [#bid ::= ctxState.bid]
              )
              scriptStateValue
          , MainSignerNoValue ctxParams.seller
          ]
        )
      ,
        ( BuyoutSpine
        ,
          [ ownInputInState WinnerSpine
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
          , #betAmount ::= lift 0
          ]
      scriptStateValue = cMinLovelace @<> ctxParams.lot

      ownInputInState :: SimpleAuctionStateSpine -> TxConstraint False SimpleAuction
      ownInputInState state =
        TxFan
          In
          (SameScript $ MkSameScriptArg $ cUpdateOfSpine' ctxState state)
          -- (SameScript $ MkSameScriptArg $ cOfSpine state [])
          -- (SameScript $ MkSameScriptArg ctxState)
          scriptStateValue
