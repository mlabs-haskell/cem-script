{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}

module Cardano.CEM.Examples.Voting where

import PlutusTx.Prelude
import Prelude qualified

import Data.Map qualified as Map

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Show.TH (deriveShow)

import Cardano.CEM
import Cardano.CEM.Stages
import Data.Spine (deriveSpine)

-- Voting

data SimpleVoting

data VoteValue = Yes | No | Abstain
  deriving stock (Prelude.Show, Prelude.Eq)

instance Eq VoteValue where
  Yes == Yes = True
  No == No = True
  Abstain == Abstain = True
  _ == _ = False

-- | Policy determinig who can vote
data JuryPolicy = Anyone | FixedJuryList [PubKeyHash] | WithToken Value
  deriving stock (Prelude.Show, Prelude.Eq)

-- Votes storage

-- | Map from jury to their decision
type VoteStorage = PMap.Map PubKeyHash VoteValue

addVote :: PubKeyHash -> VoteValue -> VoteStorage -> Either BuiltinString VoteStorage
addVote jury vote storage = case PMap.lookup jury storage of
  Just _ -> traceError "You already casted vote"
  Nothing -> Right $ PMap.insert jury vote storage

{-# INLINEABLE countVotes #-}
countVotes :: SimpleVotingParams -> VoteStorage -> VoteValue
countVotes params votesMap = maxDecision
  where
    votesFor (accum :: (Integer, Integer)) [] = accum
    votesFor (yesCount, noCount) (vote : vs) = case vote of
      Yes -> votesFor (yesCount + 1, noCount) vs
      No -> votesFor (yesCount, noCount + 1) vs
      Abstain -> votesFor (yesCount, noCount) vs
    (votesYes, votesNo) = votesFor (0, 0) $ PMap.elems votesMap
    maxDecision =
      case compare votesYes votesNo of
        GT -> Yes
        LT -> No
        EQ -> drawDecision params

-- Other datatypes

data SimpleVotingParams = MkVotingParams
  { disputeDescription :: BuiltinByteString
  , creator :: PubKeyHash
  , juryPolicy :: JuryPolicy
  , abstainAllowed :: Bool
  , drawDecision :: VoteValue
  }
  deriving stock (Prelude.Show, Prelude.Eq)

data SimpleVotingState
  = NotStarted
  | InProgress VoteStorage
  | Finalized VoteValue
  deriving stock (Prelude.Show, Prelude.Eq)

data SimpleVotingTransition
  = Create
  | Start
  | Vote PubKeyHash VoteValue
  | Finalize
  deriving stock (Prelude.Show, Prelude.Eq)

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
      [ (CreateSpine, (Always, Nothing, Just NotStartedSpine))
      , (StartSpine, (Always, Just NotStartedSpine, Just InProgressSpine))
      , (VoteSpine, (Always, Just InProgressSpine, Just InProgressSpine))
      , (FinalizeSpine, (Always, Just InProgressSpine, Nothing))
      ]

  {-# INLINEABLE transitionSpec #-}
  transitionSpec params state transition =
    case (state, transition) of
      (Nothing, Create) ->
        Right
          $ MkTransitionSpec
            { constraints = [nextScriptState NotStarted]
            , signers = [creator params]
            }
      (Just NotStarted, Start) ->
        Right
          $ MkTransitionSpec
            { constraints = [nextScriptState (InProgress PMap.empty)]
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
                nextScriptState (InProgress newVoteStorage)
                  : allowedToVoteConstraints
            , signers = [jury]
            }
      (Just (InProgress votes), Finalize) ->
        Right
          $ MkTransitionSpec
            { constraints =
                [nextScriptState $ Finalized (countVotes params votes)]
            , signers = [creator params]
            }
      _ -> Left "Wrong state transition" where
    where
      nextScriptState state' =
        MkTxFanC Out (MkTxFanFilter BySameScript (bySameCEM state')) (Exist 1)
