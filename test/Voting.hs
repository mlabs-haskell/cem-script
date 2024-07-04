module Voting (votingSpec) where

import Prelude hiding (readFile)

import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy

import GHC.IsList
import Text.Show.Pretty (ppShow)

import Plutarch.Script

import Test.Hspec (describe, it, shouldBe)

import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Examples.Voting
import Cardano.CEM.Monads
import Cardano.CEM.OffChain
import Cardano.CEM.OnChain
import Cardano.Extras (signingKeyToPKH)

import Utils

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
      -- Start
      submitAndCheck $
        MkTxSpec
          { actions = [mkAction Create]
          , specSigner = creator
          }

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

      stats <- perTransitionStats
      liftIO $ putStrLn $ ppShow stats

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

      Just state <- queryScriptState params
      liftIO $ state `shouldBe` Finalized Abstain
