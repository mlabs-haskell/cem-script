module Voting (votingSpec) where

import Cardano.CEM
import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Examples.Voting
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.Extras (signingKeyToPKH)
import Control.Monad.IO.Class (MonadIO (..))
import Test.Hspec (describe, shouldBe)
import Utils
import Prelude hiding (readFile)

votingSpec = describe "Voting" $ do
  let ignoreTest (_name :: String) = const (return ())

  -- TODO: fix Voting budget
  -- https://github.com/mlabs-haskell/cem-script/issues/108
  -- https://github.com/mlabs-haskell/clb/issues/50
  ignoreTest "Successfull flow" $
    execClb $ do
      jury1 : jury2 : creator : _ <- getTestWalletSks
      let
        params' =
          MkVotingParams
            { disputeDescription = "Test dispute"
            , creator = signingKeyToPKH creator
            , juryPolicy =
                FixedJuryList $ map signingKeyToPKH [jury1, jury2]
            , abstainAllowed = True
            , drawDecision = Abstain
            }
        params = MkCEMParams params' NoSingleStageParams
        mkAction = MkSomeCEMAction . MkCEMAction params

      -- Create
      submitAndCheck $
        MkTxSpec
          { actions = [mkAction Create]
          , specSigner = creator
          }

      -- Start
      submitAndCheck $
        MkTxSpec
          { actions = [mkAction Start]
          , specSigner = creator
          }

      -- Vote
      submitAndCheck $
        MkTxSpec
          { actions = [mkAction $ Vote (signingKeyToPKH jury1) Yes]
          , specSigner = jury1
          }

      submitAndCheck $
        MkTxSpec
          { actions = [mkAction $ Vote (signingKeyToPKH jury2) No]
          , specSigner = jury2
          }

      -- Count result
      submitAndCheck $
        MkTxSpec
          { actions = [mkAction Finalize]
          , specSigner = jury2
          }

      Just state <- queryScriptState params
      liftIO $ state `shouldBe` Finalized Abstain
