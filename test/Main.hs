{-# LANGUAGE BlockArguments #-}

module Main (main) where

import CEM.Test.Auction (auctionSpec)
import Data.Maybe (isJust)
import Test.Hspec (hspec, runIO)
import Prelude

import CEM.Test.Dynamic (dynamicSpec)
import CEM.Test.OffChain (offChainSpec)
import CEM.Test.OuraFilters.Simple (simpleSpec)
import CEM.Test.Utils (clearLogs)
import CEM.Test.Voting (votingSpec)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  runIndexing <- isJust <$> lookupEnv "INDEXING_TEST"
  hspec do
    auctionSpec
    votingSpec
    offChainSpec
    dynamicSpec
    if runIndexing
      then do
        -- These tests are not currently supported on CI
        runIO clearLogs
        simpleSpec
      else pure mempty
