{-# LANGUAGE BlockArguments #-}
module OuraFilters.Auction (spec) where
import Prelude
import Utils (SpotGarbage)
import System.Process (ProcessHandle)
import Test.Hspec (describe, it, focus)
import Test.Hspec.Core.Spec (SpecM)

spec :: SpecM (SpotGarbage IO ProcessHandle) ()
spec = 
  describe "Auction example" do
    focus $ it "Recognizes 'Create' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Start' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'MakeBid' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Close' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"
    it "Recognizes 'Buyout' transition" \spotGarbage -> do
      fail @IO @() "Not implemented"