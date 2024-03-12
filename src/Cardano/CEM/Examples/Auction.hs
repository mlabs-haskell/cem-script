{-# LANGUAGE NoPolyKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.CEM.Examples.Auction where

import Prelude qualified

import Data.Data (Proxy (..))
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Time (POSIXTime)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V2 (Address, ToData, Value)
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.Show.TH (deriveShow)

import Cardano.CEM
import Cardano.CEM.Stages
import Data.Spine

-- Simple no-deposit auction

data SimpleAuction

data Bet = MkBet
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
  | CurrentBet Bet
  | Winner Bet
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionParams = MkAuctionParams
  { seller :: PubKeyHash
  , lot :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show)

data SimpleAuctionTransition
  = Create
  | Start
  | MakeBet Bet
  | Close
  | -- TODO: discuss detirminancy
    Buyout {payingFrom :: Address}
  deriving stock (Prelude.Eq, Prelude.Show)

PlutusTx.unstableMakeIsData ''Bet
PlutusTx.unstableMakeIsData 'MkAuctionParams
PlutusTx.unstableMakeIsData 'NotStarted
PlutusTx.unstableMakeIsData 'MakeBet
PlutusTx.unstableMakeIsData ''SimpleAuctionStage
PlutusTx.unstableMakeIsData ''SimpleAuctionStageParams
deriveShow ''SimpleAuction

deriveSpine ''SimpleAuctionTransition
deriveSpine ''SimpleAuctionState

instance CEMScript SimpleAuction where
  type Stage SimpleAuction = SimpleAuctionStage
  type Params SimpleAuction = SimpleAuctionParams

  type State SimpleAuction = SimpleAuctionState

  type Transition SimpleAuction = SimpleAuctionTransition

  transitionStage Proxy =
    Map.fromList
      [ (CreateSpine, (Open, Nothing))
      , (StartSpine, (Open, Just NotStartedSpine))
      , (MakeBetSpine, (Open, Just CurrentBetSpine))
      , (CloseSpine, (Closed, Just CurrentBetSpine))
      , (BuyoutSpine, (Closed, Just WinnerSpine))
      ]

  {-# INLINEABLE transitionSpec #-}
  transitionSpec params state transition = case (state, transition) of
    (Nothing, Create) ->
      Right
        $ MkTransitionSpec
          { constraints =
              [ MkTxFanC In (MkTxFanFilter (ByPubKey $ seller params) Anything) (SumValueEq $ lot params)
              , MkTxFanC Out (MkTxFanFilter BySameScript (bySameCEM NotStarted)) (SumValueEq $ lot params)
              ]
          , signers = [seller params]
          }
    (Just NotStarted, Start) ->
      Right
        $ MkTransitionSpec
          { constraints =
              [ MkTxFanC
                  In
                  (MkTxFanFilter (ByPubKey (seller params)) Anything)
                  (SumValueEq $ lot params)
              , MkTxFanC
                  Out
                  (MkTxFanFilter BySameScript (bySameCEM (CurrentBet initialBet)))
                  (Exist 1)
              ]
          , signers = [seller params]
          }
    (Just (CurrentBet currentBet), MakeBet newBet) ->
      -- Example: could be parametrized with param or typeclass
      if betAmount newBet > betAmount currentBet
        then
          Right
            $ MkTransitionSpec
              { constraints =
                  [ MkTxFanC
                      Out
                      (MkTxFanFilter BySameScript (bySameCEM (CurrentBet newBet)))
                      (SumValueEq $ lot params)
                  ]
              , signers = [better newBet]
              }
        else Left "Wrong bet amount"
    (Just (CurrentBet currentBet), Close) ->
      Right
        $ MkTransitionSpec
          { constraints =
              saveLotConstraints
                <> [ MkTxFanC Out (MkTxFanFilter BySameScript (bySameCEM (Winner currentBet))) (Exist 1)
                   ]
          , signers = [seller params]
          }
    (Just (Winner winnerBet), Buyout {payingFrom}) ->
      Right
        $ MkTransitionSpec
          { constraints =
              [ -- Example: In constraints redundant for on-chain
                MkTxFanC
                  Out
                  (MkTxFanFilter (ByPubKey (better winnerBet)) Anything)
                  (SumValueEq $ lot params)
              , MkTxFanC
                  In
                  (MkTxFanFilter (ByPubKey (better winnerBet)) Anything)
                  (SumValueEq $ betAdaValue winnerBet)
              , MkTxFanC
                  Out
                  (MkTxFanFilter (ByPubKey (seller params)) Anything)
                  (SumValueEq $ betAdaValue winnerBet)
              ]
          , signers = [better winnerBet]
          }
    _ -> Left "Incorrect state for transition"
    where
      initialBet = MkBet (seller params) 0
      saveLotConstraints =
        [ MkTxFanC
            Out
            (MkTxFanFilter BySameScript Anything)
            (SumValueEq $ lot params)
        ]
      betAdaValue = adaValue . betAmount
      adaValue =
        singleton (CurrencySymbol emptyByteString) (TokenName emptyByteString)
