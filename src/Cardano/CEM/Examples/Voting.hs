module Cardano.CEM.Examples.Voting where

import Prelude

import Cardano.CEM
import Cardano.CEM.OnChain

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)

-- Voting

data SimpleVoting

data VoteValue = Yes | No | Abstain deriving stock (Eq)
data JuryPolicy = Anyone | FixedJuryList [PubKeyHash] | WithToken Value

-- Stub
data VoteStorage
emptyVoteStorage :: VoteStorage
emptyVoteStorage = error "Implementation is not important for example"
addVote :: PubKeyHash -> VoteValue -> VoteStorage -> Either String VoteStorage
addVote = error "Implementation is not important for example"
countVotes :: VoteStorage -> VoteValue
countVotes = error "Implementation is not important for example"

instance CEMScript SimpleVoting where
  type Stage SimpleVoting = SingleStage
  data Params SimpleVoting = MkVotingParams
    { disputeDescription :: String
    , creator :: PubKeyHash
    , juryPolicy :: JuryPolicy
    , abstainAllowed :: Bool
    }
  data State SimpleVoting
    = NotStarted
    | InProgress VoteStorage
    | Finalized VoteValue
  data Transition SimpleVoting
    = Start
    | Vote PubKeyHash VoteValue
    | Finalize

  transitionSpec params state transition =
    case (state, transition) of
      (NotStarted, Start) ->
        Right $
          MkTransitionSpec
            { stage = Always
            , сonstraints =
                [ cemScriptStateST params $ InProgress emptyVoteStorage
                ]
            , signers = [creator params]
            }
      (InProgress votes, Vote jury vote) -> do
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
                WithToken value -> [MkTxFanC InRef (ByPubKey jury) value]
                _ -> []

        -- Update state
        newVoteStorage <- addVote jury vote votes
        Right $
          MkTransitionSpec
            { stage = Always
            , сonstraints =
                [ cemScriptStateST params $ InProgress newVoteStorage
                ]
                  ++ allowedToVoteConstraints
            , signers = [jury]
            }
      (InProgress votes, Finalize) ->
        Right $
          MkTransitionSpec
            { stage = Always
            , сonstraints =
                [ cemScriptStateST params (Finalized (countVotes votes))
                ]
            , signers = [creator params]
            }
      _ -> Left "Wrong state transition"
