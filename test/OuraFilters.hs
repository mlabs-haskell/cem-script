{-# LANGUAGE BlockArguments #-}
module OuraFilters (ouraFiltersSpec) where

import Prelude

import Test.Hspec (describe, it, shouldBe, Spec)

ouraFiltersSpec :: Spec
ouraFiltersSpec = do
  it "Can execute oura" do
    fail @IO @() "not implemented"