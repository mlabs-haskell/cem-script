{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Prelude

import Test.Hspec (hspec, runIO)

import Auction (auctionSpec)
import Data.Maybe (isJust)
import Dynamic (dynamicSpec)
import OffChain (offChainSpec)
import OuraFilters (ouraFiltersSpec)
import System.Environment (lookupEnv)
import Utils (clearLogs)
import Voting (votingSpec)

main :: IO ()
main = do
  runIndexing <- isJust <$> lookupEnv "INDEXING_TEST"
  hspec do
    auctionSpec
    -- votingSpec
    -- offChainSpec
    -- dynamicSpec
    if runIndexing
      then do
        -- These tests are not currently supported on CI
        runIO clearLogs
        ouraFiltersSpec
      else pure mempty
