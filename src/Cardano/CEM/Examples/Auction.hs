module Cardano.CEM.Examples.Auction where

import PlutusTx.Prelude

import Cardano.CEM
import Cardano.CEM.OnChain

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Interval (always, from, to)
import PlutusLedgerApi.V1.Time (POSIXTime)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V2 (Value)

-- Simple no-deposit auction

data SimpleAuction

data Bet = MkBet
  { better :: PubKeyHash
  , betAmount :: Integer
  }

data SimpleAuctionStages = Open | Closed

instance Stages SimpleAuctionStages where
  data StageParams SimpleAuctionStages
    = NoControl
    | CanCloseAt POSIXTime
  stageToOnChainInterval NoControl _ = always
  -- Example: logical error
  stageToOnChainInterval (CanCloseAt time) Open = to time
  stageToOnChainInterval (CanCloseAt time) Closed = from time

instance CEMScript SimpleAuction where
  type Stage SimpleAuction = SimpleAuctionStages
  data Params SimpleAuction = MkVotingParams
    { seller :: PubKeyHash
    , lot :: Value
    }
  data State SimpleAuction
    = NotStarted
    | CurrentBet Bet
    | Winner Bet
  data Transition SimpleAuction
    = Start
    | MakeBet Bet
    | Close
    | Buyout

  transitionSpec params state transition = case (state, transition) of
    (NotStarted, Start) ->
      Right
        $ MkTransitionSpec
          { stage = Open
          , сonstraints =
              [ MkTxFanC In (ByPubKey (seller params)) (SumValueEq $ lot params)
              , MkTxFanC Out (BySameCEM (CurrentBet initialBet)) (Exist 1)
              ]
          , signers = [seller params]
          }
    (CurrentBet currentBet, MakeBet newBet) ->
      -- Example: could be parametrized with param or typeclass
      if betAmount newBet > betAmount currentBet
        then
          Right
            $ MkTransitionSpec
              { stage = Open
              , сonstraints =
                  saveLotConstraints
                    <> [ MkTxFanC Out (BySameCEM (CurrentBet newBet)) (Exist 1)
                       ]
              , signers = [better newBet]
              }
        else Left "Wrong bet amount"
    (CurrentBet currentBet, Close) ->
      Right
        $ MkTransitionSpec
          { stage = Closed
          , сonstraints =
              saveLotConstraints
                <> [ MkTxFanC Out (BySameCEM (Winner currentBet)) (Exist 1)
                   ]
          , signers = [seller params]
          }
    (Winner winnerBet, Buyout) ->
      Right
        $ MkTransitionSpec
          { stage = Closed
          , сonstraints =
              [ -- Example: In constraints redundant for on-chain
                MkTxFanC In Anything (SumValueEq $ lot params)
              , MkTxFanC Out (ByPubKey (better winnerBet)) (SumValueEq $ lot params)
              , MkTxFanC In (ByPubKey (better winnerBet)) (SumValueEq $ betAdaValue winnerBet)
              , MkTxFanC Out (ByPubKey (seller params)) (SumValueEq $ betAdaValue winnerBet)
              ]
          , signers = [better winnerBet]
          }
    _ -> Left "Incorrect stage for transition"
    where
      initialBet = MkBet (seller params) 0
      saveLotConstraints =
        [ MkTxFanC In Anything (SumValueEq $ lot params)
        , MkTxFanC Out Anything (SumValueEq $ lot params)
        ]
      betAdaValue = adaValue . betAmount
      adaValue =
        singleton (CurrencySymbol emptyByteString) (TokenName emptyByteString)
