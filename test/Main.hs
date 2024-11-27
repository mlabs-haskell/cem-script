{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Prelude

import Test.Hspec (hspec, runIO)

import Auction (auctionSpec)
import Dynamic (dynamicSpec)
import OffChain (offChainSpec)
import OuraFilters (ouraFiltersSpec)
import Utils (clearLogs)
import Voting (votingSpec)

main :: IO ()
main = hspec do
  -- auctionSpec
  -- votingSpec
  -- offChainSpec
  -- dynamicSpec
  runIO clearLogs
  ouraFiltersSpec
