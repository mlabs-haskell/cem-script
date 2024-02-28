{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Cardano.CEM.Examples.Voting where

import PlutusTx.Prelude
import Prelude qualified

import Data.Map qualified as Map

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)
import PlutusTx qualified
import PlutusTx.Show.TH (deriveShow)

import Cardano.Api.Ledger (Vote)
import Cardano.CEM
import Cardano.CEM.Stages
import Data.Spine (deriveSpine)

-- Voting

data SimpleVoting

data VoteValue = Yes | No | Abstain

instance Eq VoteValue where
  Yes == Yes = True
  No == No = True
  Abstain == Abstain = True
  _ == _ = False

-- TODO
data JuryPolicy = Anyone | FixedJuryList [PubKeyHash] | WithToken Value

-- Stub

data VoteStorage = MkVoteStorage

emptyVoteStorage :: VoteStorage
emptyVoteStorage = MkVoteStorage

addVote :: PubKeyHash -> VoteValue -> VoteStorage -> Either BuiltinString VoteStorage
addVote = traceError "Implementation is not important for example"

countVotes :: VoteStorage -> VoteValue
countVotes = traceError "Implementation is not important for example"

data SimpleVotingParams = MkVotingParams
  { disputeDescription :: BuiltinByteString
  , creator :: PubKeyHash
  , juryPolicy :: JuryPolicy
  , abstainAllowed :: Bool
  }

data SimpleVotingState
  = NotStarted
  | InProgress VoteStorage
  | Finalized VoteValue

data SimpleVotingTransition
  = Start
  | Vote PubKeyHash VoteValue
  | Finalize

PlutusTx.unstableMakeIsData ''VoteStorage
PlutusTx.unstableMakeIsData ''VoteValue
PlutusTx.unstableMakeIsData ''JuryPolicy
PlutusTx.unstableMakeIsData ''SimpleVotingState
PlutusTx.unstableMakeIsData ''SimpleVotingParams
PlutusTx.unstableMakeIsData ''SimpleVotingTransition

deriveShow ''SimpleVoting

deriveSpine ''SimpleVotingTransition
deriveSpine ''SimpleVotingState

instance CEMScript SimpleVoting where
  type Stage SimpleVoting = SingleStage
  type Params SimpleVoting = SimpleVotingParams
  type State SimpleVoting = SimpleVotingState
  type Transition SimpleVoting = SimpleVotingTransition

  transitionStage _ =
    Map.fromList
      [ (StartSpine, (Always, Just NotStartedSpine))
      , (VoteSpine, (Always, Just InProgressSpine))
      , (FinalizeSpine, (Always, Just InProgressSpine))
      ]

  {-# INLINEABLE transitionSpec #-}
  transitionSpec params state transition =
    case (state, transition) of
      (Just NotStarted, Start) ->
        Right
          $ MkTransitionSpec
            { constraints =
                [ MkTxFanC
                    Out
                    (MkTxFanFilter BySameScript (bySameCEM $ InProgress emptyVoteStorage))
                    (Exist 1)
                ]
            , signers = [creator params]
            }
      (Just (InProgress votes), Vote jury vote) -> do
        -- Check if you can vote
        case juryPolicy params of
          FixedJuryList allowedJury ->
            if jury `notElem` allowedJury
              then Left "You are not allowed to vote, not on list"
              else return ()
          _ -> return ()
        if not (abstainAllowed params) && vote == Abstain
          then Left "You cannot vote Abstain in this vote"
          else return ()

        let allowedToVoteConstraints =
              case juryPolicy params of
                WithToken value ->
                  [ MkTxFanC
                      InRef
                      (MkTxFanFilter (ByPubKey jury) Anything)
                      (SumValueEq value)
                  ]
                _ -> []

        -- Update state
        newVoteStorage <- addVote jury vote votes
        Right
          $ MkTransitionSpec
            { constraints =
                [ MkTxFanC
                    Out
                    (MkTxFanFilter BySameScript (bySameCEM $ InProgress newVoteStorage))
                    (Exist 1)
                ]
                  ++ allowedToVoteConstraints
            , signers = [jury]
            }
      (Just (InProgress votes), Finalize) ->
        Right
          $ MkTransitionSpec
            { constraints =
                [ MkTxFanC
                    Out
                    (MkTxFanFilter BySameScript (bySameCEM $ Finalized (countVotes votes)))
                    (Exist 1)
                ]
            , signers = [creator params]
            }
      _ -> Left "Wrong state transition" where
