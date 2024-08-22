{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Prelude

import Test.Hspec (hspec,runIO)

import Auction (auctionSpec)
import Dynamic (dynamicSpec)
import OffChain (offChainSpec)
import OuraFilters (ouraFiltersSpec)
import Voting (votingSpec)
import Utils (clearLogs)

main :: IO ()
main = hspec do
  runIO clearLogs
  ouraFiltersSpec
  dynamicSpec
  offChainSpec
  auctionSpec
  votingSpec
