module Cardano.CEM.Examples.Auction where

import Prelude

import Cardano.CEM

import PlutusLedgerApi.V2 (Value, POSIXTime, from, to, always)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)

-- Simple no-deposit auction

data SimpleAuction

data Bet = MkBet {
    better :: PubKeyHash,
    betAmount :: Integer
}

data SimpleAuctionStages = Open | Closed

instance Stages SimpleAuctionStages where
    data StageParams SimpleAuctionStages =
        NoControl | CanCloseAt POSIXTime
    stageToOnChainInterval NoControl _ = always
    -- Example: logical error
    stageToOnChainInterval (CanCloseAt time) Open = to time
    stageToOnChainInterval (CanCloseAt time) Closed = from time


instance CEMScript SimpleAuction where
    type Stage SimpleAuction = SimpleAuctionStages
    data Params SimpleAuction = MkVotingParams {
        seller :: PubKeyHash,
        lot :: Value
    }
    data State SimpleAuction =
        NotStarted | CurrentBet Bet | Winner Bet
    data Transition SimpleAuction =
        Start | MakeBet Bet | Close | Buyout

    transitionSpec params state transition = case (state, transition) of
        (NotStarted, Start) -> Right $ MkTransitionSpec {
            stage = Open,
            сonstraints = [
                MkTxFanC In (ByPubKey (seller params)) (lot params),
                cemScriptStateST params (CurrentBet initialBet)
            ],
            signers = [seller params]
        }
        (CurrentBet currentBet, MakeBet newBet) ->
            -- Example: could be parametrized with param or typeclass
            if betAmount newBet > betAmount currentBet
            then
                Right $ MkTransitionSpec {
                    stage = Open,
                    сonstraints = [
                        saveLotConstraint,
                        cemScriptStateST params (CurrentBet newBet)
                    ],
                    signers = [better newBet]
                }
            else Left "Wrong bet amount"
        (CurrentBet currentBet, Close) ->
            Right $ MkTransitionSpec {
                stage = Closed,
                сonstraints = [
                    saveLotConstraint,
                    cemScriptStateST params (Winner currentBet)
                ],
                signers = [seller params]
            }
        (Winner winnerBet, Buyout) ->
            Right $ MkTransitionSpec {
                stage = Closed,
                сonstraints = [
                    -- Example: In constraints redundant for on-chain
                    MkTxFanC In Anything (lot params),
                    MkTxFanC Out (ByPubKey (better winnerBet)) (lot params),
                    MkTxFanC In (ByPubKey (better winnerBet)) (betAdaValue winnerBet),
                    MkTxFanC Out (ByPubKey (seller params)) (betAdaValue winnerBet)
                ],
                signers = [better winnerBet]
            }
        _ -> Left "Incorrect stage for transition"
        where
            initialBet = MkBet (seller params) 0
            saveLotConstraint = MkTxFanC InAndOut Anything (lot params)
            betAdaValue = error "TODO"
