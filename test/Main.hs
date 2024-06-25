module Main (main) where

import Prelude

import Test.Hspec (hspec)

import Auction (auctionSpec)
import Dynamic (dynamicSpec)
import OffChain (offChainSpec)
import Voting (votingSpec)

main :: IO ()
main = hspec $ do
  offChainSpec
  auctionSpec
  votingSpec
  dynamicSpec
