{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cardano.CEM.Examples.Auction where

import PlutusTx.Prelude
import Prelude qualified

import Data.Data (Proxy (..))
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Time (POSIXTime)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V2 (Value)
import PlutusTx qualified

import Cardano.CEM
import Cardano.CEM (ConstraintDSL (OfSpine), TxFanConstraint (MkTxFanC), TxFanConstraint' (AdditionalSigner), TxFanFilterNew (SameScript, UserAddress))
import Cardano.CEM.Stages (Stages (..))
import Cardano.CEM.TH (deriveCEMAssociatedTypes, deriveStageAssociatedTypes)
import Data.Spine

-- Simple no-deposit auction

data SimpleAuction

data Bid = MkBet
  { better :: PubKeyHash
  , betAmount :: Integer
  }
  deriving stock (Prelude.Eq, Prelude.Show)

deriveSpine ''Bid

data SimpleAuctionStage = Open | Closed
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionStageParams
  = NoControl
  | CanCloseAt POSIXTime
  deriving stock (Prelude.Eq, Prelude.Show)

instance Stages SimpleAuctionStage SimpleAuctionStageParams where
  type StageParams SimpleAuctionStage = SimpleAuctionStageParams
  stageToOnChainInterval NoControl _ = Interval.always
  -- Example: logical error
  stageToOnChainInterval (CanCloseAt time) Open = Interval.to time
  stageToOnChainInterval (CanCloseAt time) Closed = Interval.from time

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

PlutusTx.unstableMakeIsData ''Bid

instance CEMScriptTypes SimpleAuction where
  type Stage SimpleAuction = SimpleAuctionStage
  type Params SimpleAuction = SimpleAuctionParams
  type State SimpleAuction = SimpleAuctionState
  type Transition SimpleAuction = SimpleAuctionTransition

$(deriveStageAssociatedTypes ''SimpleAuctionStage)
$(deriveCEMAssociatedTypes False ''SimpleAuction)

instance CEMScript SimpleAuction where
  transitionStage Proxy =
    Map.fromList
      [ (CreateSpine, (Open, Nothing, Just NotStartedSpine))
      , (StartSpine, (Open, Just NotStartedSpine, Just CurrentBidSpine))
      , (MakeBidSpine, (Open, Just CurrentBidSpine, Just CurrentBidSpine))
      , (CloseSpine, (Closed, Just CurrentBidSpine, Just WinnerSpine))
      , (BuyoutSpine, (Closed, Just WinnerSpine, Nothing))
      ]

  transitionSpec' =
    Map.fromList
      [
        ( CreateSpine
        ,
          [ TxFan In (UserAddress ctxParams.seller) ctxParams.lot
          , TxFan Out (SameScript $ Pure NotStarted) minLovelace
          ]
        )
      ,
        ( StartSpine
        ,
          [ TxFan In (SameScript $ Pure NotStarted) minLovelace
          , TxFan
              Out
              ( SameScript
                  $ OfSpine CurrentBidSpine [#bid ::= initialBid]
              )
              minLovelace
          , AdditionalSigner ctxParams.seller
          ]
        )
      ,
        ( MakeBidSpine
        ,
          [ Embed
              $ If
                (ctxState.bid.betAmount @>= ctxTransition.bid.betAmount)
                (Error "Wrong Bid amount")
                (Check Noop)
          , TxFan
              Out
              ( SameScript
                  $ OfSpine
                    CurrentBidSpine
                    [#bid ::= ctxTransition.bid]
              )
              minLovelace
          , AdditionalSigner ctxTransition.bid.better
          ]
        )
      ,
        ( CloseSpine
        ,
          [ TxFan
              Out
              ( SameScript
                  $ OfSpine WinnerSpine [#bid ::= ctxState.bid]
              )
              minLovelace
          , AdditionalSigner ctxParams.seller
          ]
        )
      ,
        ( BuyoutSpine
        ,
          [ checkOnchainOnly
              ( TxFan In (UserAddress buyoutBid.better)
                  $ AdaValue buyoutBid.betAmount
              )
          , TxFan In (UserAddress buyoutBid.better) ctxParams.lot
          , TxFan Out (UserAddress buyoutBid.better) ctxParams.lot
          ]
        )
      ]
    where
      buyoutBid = ctxState.winnerBet
      initialBid =
        OfSpine
          MkBetSpine
          [ #better ::= ctxParams.seller
          , #betAmount ::= Pure 0
          ]

  {-# INLINEABLE transitionSpec #-}
  transitionSpec params state transition = case (state, transition) of
    (Nothing, Create) ->
      Right
        $ MkTransitionSpec
          { constraints =
              [ MkTxFanC
                  In
                  (MkTxFanFilter (ByPubKey $ seller params) Anything)
                  (SumValueEq $ lot params)
              , nextState NotStarted
              ]
          , signers = []
          }
    (Just NotStarted, Start) ->
      Right
        $ MkTransitionSpec
          { constraints = [nextState (CurrentBid initialBid)]
          , signers = [seller params]
          }
    (Just (CurrentBid currentBet), MakeBid newBet) ->
      -- Example: could be parametrized with param or typeclass
      if betAmount newBet > betAmount currentBet
        then
          Right
            $ MkTransitionSpec
              { constraints = [nextState (CurrentBid newBet)]
              , signers = [better newBet]
              }
        else Left "Wrong Bid amount"
    (Just (CurrentBid currentBet), Close) ->
      Right
        $ MkTransitionSpec
          { constraints = [nextState (Winner currentBet)]
          , signers = [seller params]
          }
    (Just (Winner winnerBet), Buyout {}) ->
      Right
        $ MkTransitionSpec
          { constraints =
              [ -- Example: In constraints redundant for on-chain
                MkTxFanC
                  In
                  (MkTxFanFilter (ByPubKey (better winnerBet)) Anything)
                  (SumValueEq $ betAdaValue winnerBet)
              , MkTxFanC
                  Out
                  (MkTxFanFilter (ByPubKey (better winnerBet)) Anything)
                  (SumValueEq $ lot params)
              , MkTxFanC
                  Out
                  (MkTxFanFilter (ByPubKey (seller params)) Anything)
                  (SumValueEq $ betAdaValue winnerBet)
              ]
          , signers = []
          }
    _ -> Left "Incorrect state for transition"
    where
      initialBid = MkBet (seller params) 0
      nextState state' =
        MkTxFanC
          Out
          (MkTxFanFilter BySameScript (bySameCEM state'))
          (SumValueEq $ lot params)
      betAdaValue = adaValue . betAmount
      adaValue =
        singleton (CurrencySymbol emptyByteString) (TokenName emptyByteString)
