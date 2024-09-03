{-# LANGUAGE NoPolyKinds #-}

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
import Cardano.CEM.Stages (Stages (..))
import Cardano.CEM.TH (deriveCEMAssociatedTypes, deriveStageAssociatedTypes)

-- Simple no-deposit auction

data SimpleAuction

data Bid = MkBet
  { better :: PubKeyHash
  , betAmount :: Integer
  }
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionStage = Open | Closed
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionStageParams
  = NoControl
  | CanCloseAt POSIXTime
  deriving stock (Prelude.Eq, Prelude.Show)

instance Stages SimpleAuctionStage where
  type StageParams SimpleAuctionStage = SimpleAuctionStageParams
  stageToOnChainInterval NoControl _ = Interval.always
  -- Example: logical error
  stageToOnChainInterval (CanCloseAt time) Open = Interval.to time
  stageToOnChainInterval (CanCloseAt time) Closed = Interval.from time

data SimpleAuctionState
  = NotStarted
  | CurrentBid Bid
  | Winner Bid
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionParams = MkAuctionParams
  { seller :: PubKeyHash
  , lot :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionTransition
  = Create
  | Start
  | MakeBid Bid
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

  {-# INLINEABLE transitionSpec #-}
  transitionSpec params state transition = case (state, transition) of
    (Nothing, Create) ->
      Right
        $ MkTransitionSpec
          { constraints =
              [ MkTxFansC
                  In
                  (MkTxFanFilter (ByPubKey $ seller params) AnyDatum)
                  (FansWithTotalValueOfAtLeast $ lot params)
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
                MkTxFansC
                  In
                  (MkTxFanFilter (ByPubKey (better winnerBet)) AnyDatum)
                  (FansWithTotalValueOfAtLeast $ betAdaValue winnerBet)
              , MkTxFansC
                  Out
                  (MkTxFanFilter (ByPubKey (better winnerBet)) AnyDatum)
                  (FansWithTotalValueOfAtLeast $ lot params)
              , MkTxFansC
                  Out
                  (MkTxFanFilter (ByPubKey (seller params)) AnyDatum)
                  (FansWithTotalValueOfAtLeast $ betAdaValue winnerBet)
              ]
          , signers = []
          }
    _ -> Left "Incorrect state for transition"
    where
      initialBid = MkBet (seller params) 0
      nextState state' =
        MkTxFansC
          Out
          (MkTxFanFilter BySameScript (bySameCEM state'))
          (FansWithTotalValueOfAtLeast $ lot params)
      betAdaValue = adaValue . betAmount
      adaValue =
        singleton (CurrencySymbol emptyByteString) (TokenName emptyByteString)
