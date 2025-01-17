module CEM.Test.Voting (votingSpec) where

import CEM.Example.Compiled ()
import CEM.Example.Voting
import CEM.Test.Utils
import Cardano.CEM
import Cardano.Extras (signingKeyToPKH)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy
import GHC.IsList
import Plutarch.Script
import Test.Hspec (describe, it, shouldBe)
import Prelude hiding (readFile)

-- import Text.Show.Pretty (ppShow)

votingSpec = describe "Voting" $ do
  it "Serialise" $ do
    let !script = cemScriptCompiled (Proxy :: Proxy SimpleVoting)
    putStrLn $
      "Script bytes: "
        <> show (length $ toList $ serialiseScript script)
  it "Successful flow" $
    execClb $ do
      jury1 : jury2 : creator : _ <- getTestWalletSks
      let
        params =
          MkVotingParams
            { disputeDescription = "Test dispute"
            , creator = signingKeyToPKH creator
            , juryPolicy =
                FixedJuryList $ map signingKeyToPKH [jury1, jury2]
            , abstainAllowed = True
            , drawDecision = Abstain
            }
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

      -- stats <- perTransitionStats
      -- liftIO $ putStrLn $ ppShow stats

      submitAndCheck $
        MkTxSpec
          { actions = [mkAction $ Vote (signingKeyToPKH jury2) No]
          , specSigner = jury2
          }

      -- Count result
      submitAndCheck $
        MkTxSpec
          { actions = [mkAction Finalize]
          , specSigner = creator
          }

      -- stats <- perTransitionStats
      -- liftIO $ putStrLn $ ppShow stats

      Just state <- queryScriptState params
      liftIO $ state `shouldBe` Finalized Abstain
