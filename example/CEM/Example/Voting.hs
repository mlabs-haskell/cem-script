{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use when" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | CEM Script Voting example
module CEM.Example.Voting where

import Cardano.CEM
import Data.Map qualified as Map
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (Value)
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Prelude
import Prelude qualified

data SimpleVoting

data VoteValue = Yes | No | Abstain
  deriving stock (Prelude.Show, Prelude.Eq)

derivePlutusSpine ''VoteValue

instance Eq VoteValue where
  Yes == Yes = True
  No == No = True
  Abstain == Abstain = True
  _ == _ = False

-- | Policy determining who can vote
data JuryPolicy
  = Anyone
  | FixedJuryList
      { juryList :: [PubKeyHash]
      }
  | WithToken
      { juryAuthTokenValue :: Value
      }
  deriving stock (Prelude.Show, Prelude.Eq)

derivePlutusSpine ''JuryPolicy

-- Votes storage

-- | Map from jury to their decision
type VoteStorage = PMap.Map PubKeyHash VoteValue

data VoteAddResult
  = DuplicateVote
  | Success {newVoteStorage :: VoteStorage}
  deriving stock (Prelude.Eq, Prelude.Show)

derivePlutusSpine ''VoteAddResult

addVote :: PubKeyHash -> VoteValue -> VoteStorage -> VoteAddResult
addVote jury vote storage = case PMap.lookup jury storage of
  Nothing -> Success $ PMap.insert jury vote storage
  Just {} -> DuplicateVote

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
  | InProgress
      { votes :: VoteStorage
      }
  | Finalized
      { votingResult :: VoteValue
      }
  deriving stock (Prelude.Show, Prelude.Eq)

data SimpleVotingTransition
  = Create
  | Start
  | Vote
      { votingJury :: PubKeyHash
      , voteValue :: VoteValue
      }
  | Finalize
  deriving stock (Prelude.Show, Prelude.Eq)

data SimpleVotingCalc
  = VoteCalc
      { votingNotAllowed :: Bool
      , voteAddResult :: VoteAddResult
      }
  | FinalizeCalc {result :: VoteValue}
  | NoCalc
  deriving stock (Prelude.Eq, Prelude.Show)

instance CEMScriptTypes SimpleVoting where
  type Params SimpleVoting = SimpleVotingParams
  type State SimpleVoting = SimpleVotingState
  type Transition SimpleVoting = SimpleVotingTransition
  type TransitionComp SimpleVoting = SimpleVotingCalc

derivePlutusSpine ''SimpleVotingCalc
$(deriveCEMAssociatedTypes False ''SimpleVoting)

instance CEMScript SimpleVoting where
  compilationConfig = MkCompilationConfig "VOT"

  {-# INLINEABLE transitionComp #-}
  transitionComp ::
    Maybe
      ( Params SimpleVoting ->
        State SimpleVoting ->
        Transition SimpleVoting ->
        TransitionComp SimpleVoting
      )
  transitionComp = Just go
    where
      go params (InProgress votes) transition =
        case transition of
          Vote jury vote ->
            VoteCalc
              { votingNotAllowed =
                  case juryPolicy params of
                    FixedJuryList allowedJury ->
                      jury `notElem` allowedJury
                    _ -> False
              , voteAddResult = addVote jury vote votes
              }
          Finalize -> FinalizeCalc $ countVotes params votes
          _ -> NoCalc
      go _ _ _ = NoCalc

  perTransitionScriptSpec =
    Map.fromList
      [
        ( CreateSpine
        ,
          [ TxFan Out (SameScript $ MkSameScriptArg $ lift NotStarted) cMinLovelace
          , MainSignerNoValue ctxParams.creator
          ]
        )
      ,
        ( StartSpine
        ,
          [ TxFan In (SameScript $ MkSameScriptArg $ lift NotStarted) cMinLovelace
          , TxFan Out (SameScript $ MkSameScriptArg $ lift $ InProgress PMap.empty) cMinLovelace
          , MainSignerNoValue ctxParams.creator
          ]
        )
      ,
        ( VoteSpine
        ,
          [ sameScriptIncOfSpine InProgressSpine
          , MatchBySpine ctxComp.voteAddResult
              $ Map.fromList
                [ (DuplicateVoteSpine, Error "You already casted vote")
                ,
                  ( SuccessSpine
                  , TxFan
                      Out
                      ( SameScript
                          $ MkSameScriptArg
                          $ cOfSpine
                            InProgressSpine
                            [ #votes
                                ::= ctxComp.voteAddResult.newVoteStorage
                            ]
                      )
                      cMinLovelace
                  )
                ]
          , MainSignerNoValue ctxTransition.votingJury
          , MatchBySpine ctxParams.juryPolicy
              $ Map.fromList
                [
                  ( WithTokenSpine
                  , TxFan
                      InRef
                      (UserAddress ctxTransition.votingJury)
                      ctxParams.juryPolicy.juryAuthTokenValue
                  )
                , (FixedJuryListSpine, Noop)
                , (AnyoneSpine, Noop)
                ]
          , byFlagError
              ctxComp.votingNotAllowed
              "You are not allowed to vote, not on list"
          , byFlagError
              ( cNot ctxParams.abstainAllowed
                  @&& (ctxTransition.voteValue @== lift Abstain)
              )
              "You cannot vote Abstain in this vote"
          ]
        )
      ,
        ( FinalizeSpine
        ,
          [ sameScriptIncOfSpine InProgressSpine
          , TxFan
              Out
              ( SameScript
                  $ MkSameScriptArg
                  $ cOfSpine
                    FinalizedSpine
                    [#votingResult ::= ctxComp.result]
              )
              cMinLovelace
          , MainSignerNoValue ctxParams.creator
          ]
        )
      ]
    where
      sameScriptIncOfSpine spine =
        TxFan In (SameScript $ MkSameScriptArg $ cUpdateOfSpine ctxState spine []) cMinLovelace
